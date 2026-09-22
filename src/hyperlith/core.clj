(ns hyperlith.core
  (:refer-clojure :exclude [parse-long])
  (:require
   [aleph.http :as http]
   [clojure.main :refer [repl-caught]]
   [hyperlith.impl.assets]
   [hyperlith.impl.blocker :refer [wrap-blocker]]
   [hyperlith.impl.codec :as codec]
   [hyperlith.impl.crypto :as crypto]
   [hyperlith.impl.css]
   [hyperlith.impl.datastar :as ds]
   [hyperlith.impl.env]
   [hyperlith.impl.html :as h]
   [hyperlith.impl.json :refer [wrap-parse-json-body]]
   [hyperlith.impl.lane-context :as lc]
   [hyperlith.impl.namespaces :refer [import-vars]]
   [hyperlith.impl.params :refer [wrap-query-params]]
   [hyperlith.impl.router :as router]
   [hyperlith.impl.session :refer [wrap-session]]
   [hyperlith.impl.sqlite :as sqlite]
   [hyperlith.impl.trace]
   [hyperlith.impl.util :as u]
   [ol.clave.ext.aleph :as clave-aleph])
  (:import
   (hyperlith.impl.lane_context LaneCtx)
   (java.net ServerSocket)
   [java.nio ByteBuffer]
   (java.util ArrayList Map$Entry)
   (java.util.concurrent
    ConcurrentHashMap
    Executors
    LinkedBlockingQueue
    Semaphore)
   [java.util.concurrent.atomic AtomicInteger]))

(import-vars
  ;; ENV
  [hyperlith.impl.env
   env]
  ;; UTIL
  [hyperlith.impl.util
   load-resource
   parse-long
   modulo-pick]
  ;; HTML
  [hyperlith.impl.html
   html->bytes
   html->bytes-oneshot]
  ;; CRYPTO
  [hyperlith.impl.crypto
   new-uid
   digest]
  ;; DATASTAR
  [hyperlith.impl.datastar
   patch-signals
   execute-expr]
  ;; CSS
  [hyperlith.impl.css
   static-css
   --]
  ;; ASSETS
  [hyperlith.impl.assets
   static-asset]
  ;; TRACE
  [hyperlith.impl.trace
   traces
   trace>
   traces-reset!]
  ;; CODEC
  [hyperlith.impl.codec
   url-query-string
   url-encode]
  ;; JSON
  [hyperlith.impl.json
   json->edn
   edn->json])

(defmacro defaction
  {:clj-kondo/lint-as 'clojure.core/defn}
  [sym args & body]
  (let [path   (str "/" (crypto/digest (str *ns* "/" sym)))
        sym-fn (symbol (str sym "-fn"))]
    `(do (defn ~sym-fn ~args ~@body)
         (ds/action-handler ~path (var ~sym-fn))
         (def ~sym ~path))))

(defmacro defview
  {:clj-kondo/lint-as 'clojure.core/defn}
  [sym {:keys [path shim-headers] :as opts} args & body]
  (let [sym-fn (symbol (str sym "-fn"))]
    `(do (defn ~sym-fn ~args ~@body)
         (ds/shim-handler ~path ~shim-headers)
         (ds/render-handler ~path (var ~sym-fn) ~opts)
         (def ~sym ~path))))

(defn throw-if-port-in-use! [port]
  (try
    (with-open [_ (ServerSocket. port)])
    (catch Throwable _
      (throw
        (ex-info
          (str "Port "port
            " already in use! Server might already be runnin!")
          {:port port})))))

(defn wrap-error [handler]
  (fn [req]
    (try
      (handler req)
      (catch Throwable t
        (repl-caught t)
        {:status 500}))))

(defn start-batch-loop!
  [ctx {:keys [batch-fn batch-tick-ms dbs lanes start-sem done-sem]}]
  (assert (not (nil? batch-tick-ms)))
  (assert (not (nil? batch-fn)))
  (assert (not (nil? dbs)))
  (assert (not (nil? lanes)))
  (assert (not (nil? start-sem)))
  (assert (not (nil? done-sem)))
  (let [q       (LinkedBlockingQueue/new)
        ctx     (merge ctx (sqlite/create-write-connections! dbs))
        n-lanes (count lanes)
        t       (Thread.
                  ^Runnable
                  (bound-fn* ;; binding conveyance
                    (fn batch-thread []
                      (while (not (Thread/interrupted))
                        (let [next-tick (+ (System/currentTimeMillis)
                                          batch-tick-ms)
                              batch     (ArrayList/new)]
                          (.drainTo q batch)
                          (try
                            (batch-fn ctx (seq batch))
                            ;; Refresh connections
                            (Semaphore/.release start-sem n-lanes)
                            (Semaphore/.acquire done-sem n-lanes)
                            (catch Throwable t
                              (repl-caught t)
                              (flush)))
                          (let [sleep-time-ms (- next-tick
                                                (System/currentTimeMillis))]
                            (when (> sleep-time-ms 0)
                              (Thread/sleep ^long sleep-time-ms))))))))
        _       (Thread/.start t)]
    (-> (assoc ctx
          ::tx!
          (fn tx! [thunk] (LinkedBlockingQueue/.offer q thunk)) )
      (update ::stop!
        conj (fn [] (Thread/.interrupt t))))))

(defn- init-render-lanes
  [{:keys [render-pool-size dbs render-buffer-size start-sem done-sem]}]
  (assert (not (nil? render-pool-size)))
  (assert (not (nil? render-buffer-size)))
  (assert (not (nil? dbs)))
  (assert (not (nil? start-sem)))
  (assert (not (nil? done-sem)))
  (->> (range render-pool-size)
    (mapv (fn [_]
            (let [lane-ctx ^LaneCtx
                  (lc/new-lane-ctx
                    {:dbs          (sqlite/create-read-connections! dbs)
                     :lane-conns   (ConcurrentHashMap.)
                     :html-dst-buf (ByteBuffer/allocate render-buffer-size)
                     ;; zstd is in native lang
                     :zstd-src-buf (ByteBuffer/allocateDirect
                                     render-buffer-size)})]
              (-> (Thread.
                    ^Runnable
                    (bound-fn* ;; binding conveyance
                      (fn render-thread []
                        (while (not (Thread/interrupted))
                          (Semaphore/.acquire start-sem)
                          (let [conns ^ConcurrentHashMap
                                (.lane-conns lane-ctx)]
                            (run! sqlite/start-read-tx
                              (vals (.dbs lane-ctx)))
                            (run! (fn [conn]
                                    ((Map$Entry/.getValue conn)))
                              (.entrySet conns))
                            (run! sqlite/end-read-tx
                              (vals (.dbs lane-ctx))))
                          (Semaphore/.release done-sem)))))
                Thread/.start)
              lane-ctx)))))

(defn start-app
  [{:keys [port ctx-start batch-fn batch-tick-ms
           domain email dev? dbs render-pool-size render-buffer-size]
    :or   {port               8080
           batch-tick-ms      50
           ctx-start          (fn [] {})
           render-pool-size   (Runtime/.availableProcessors
                                (Runtime/getRuntime))
           render-buffer-size (* 32 16384)}}]
  (let [port        (if dev? port 443)
        start-sem   (Semaphore/new render-pool-size true)
        done-sem    (Semaphore/new render-pool-size true)
        lanes       (init-render-lanes
                      {:render-pool-size   render-pool-size
                       :render-buffer-size render-buffer-size
                       :dbs                dbs
                       :start-sem          start-sem
                       :done-sem           done-sem})
        select-lane (let [lane-idx ^AtomicInteger (AtomicInteger. 0)]
                      ;; Round robin lane select
                      (fn ^LaneCtx []
                        (get lanes
                          (Math/floorMod (.getAndIncrement lane-idx)
                            render-pool-size))))
        _           (throw-if-port-in-use! port)
        ctx         (-> (ctx-start)
                      (assoc ::select-lane select-lane)
                      (start-batch-loop!
                        {:lanes         lanes
                         :dbs           dbs
                         :batch-fn      batch-fn
                         :batch-tick-ms batch-tick-ms
                         :start-sem     start-sem
                         :done-sem      done-sem}))
        wrap-ctx    (fn [handler]
                      (fn [req]
                        (handler (u/fast-merge req ctx))))
        ;; Middleware make for messy error stacks.
        router      (-> router/router
                      wrap-ctx
                      ;; Wrap error here because req params/body/session
                      ;; have been handled (and provide useful context).
                      wrap-error
                      ;; The handlers after this point do not throw errors
                      ;; are robust/lenient.
                      wrap-query-params
                      wrap-session
                      wrap-parse-json-body
                      wrap-blocker)
        config      {:executor              (Executors/newVirtualThreadPerTaskExecutor)
                     :port                  port
                     ;; Actions payloads are small
                     :max-request-body-size 4096
                     :request-buffer-size   4096}
        server
        (if dev?
          (http/start-server router config)
          (clave-aleph/start-server router
            (merge
              config
              {:http-versions             [:http2 :http1]
               ::clave-aleph/http-options {:port 80}
               ::clave-aleph/config
               {:domains [domain]
                :issuers
                [{:directory-url
                  "https://acme-v02.api.letsencrypt.org/directory"
                  :email email}]}})))]
    {:wrapped-router router
     :ctx            ctx
     :stop!          (fn stop [& [_opts]]
                       (clave-aleph/stop server)
                       (->> ctx ::stop!
                         (run! (fn [stop!] (stop!)))))}))



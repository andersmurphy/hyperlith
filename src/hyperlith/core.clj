(ns hyperlith.core
  (:refer-clojure :exclude [parse-long])
  (:require
   [aleph.http :as http]
   [clojure.main :refer [repl-caught]]
   [hyperlith.impl.assets]
   [hyperlith.impl.blocker :refer [wrap-blocker]]
   [hyperlith.impl.cache :as cache]
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
   (java.util ArrayList HashMap)
   (java.util.concurrent
    Callable
    ConcurrentHashMap
    Executors
    LinkedBlockingQueue
    ThreadPoolExecutor)))

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
   escape]
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
    (with-open [_ (ServerSocket. 8080)])
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

(defn- init-render-lanes [n-lanes dbs]
  (->> (range n-lanes)
    (mapv (fn [_]
            (lc/map->LaneCtx
              {:dbs               (sqlite/create-read-connections! dbs)
               :attr-cache        (cache/init 2000)
               :attr-name-cache   (HashMap.)
               ;; Goes back onto the heap when converted to byte array
               :attr-byte-scratch (ByteBuffer/allocate 16384)
               :html-dst-buf      (ByteBuffer/allocate (* 32 16384))
               ;; zstd is in native lang
               :zstd-src-buf      (ByteBuffer/allocateDirect (* 32 16384))})))))

(defn start-batch-loop!
  [{:keys [::conns] :as ctx}
   {:keys [batch-fn batch-tick-ms dbs]}]
  (let [q       (LinkedBlockingQueue/new)
        ctx     (merge ctx (sqlite/create-write-connections! dbs))
        n-lanes (Runtime/.availableProcessors (Runtime/getRuntime))
        lanes   (init-render-lanes n-lanes dbs)
        pool    (Executors/newFixedThreadPool n-lanes)
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
                            (let [vs   (->> (ConcurrentHashMap/.entrySet conns)
                                         (mapv java.util.Map$Entry/.getValue))
                                  n-vs (count vs)]
                              (->> (range n-lanes)
                                (mapv (fn [i]
                                        ^Callable
                                        (fn []
                                          (let [lane-ctx ^LaneCtx (get lanes i)]
                                            (run! sqlite/start-read-tx
                                              (vals (.dbs lane-ctx)))
                                            (loop [k 0]
                                              (let [idx (+ i (* n-lanes k))]
                                                (when (< idx n-vs)
                                                  ((nth vs idx) lane-ctx)
                                                  (recur (inc k)))))
                                            (run! sqlite/end-read-tx
                                              (vals (.dbs lane-ctx)))))))
                                (ThreadPoolExecutor/.invokeAll pool)))
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

(defn start-app
  [{:keys [port ctx-start batch-fn batch-tick-ms
           domain email dev? dbs]
    :or   {port      8080 batch-tick-ms 50 ctx-start (fn [] {})}}]
  (assert (not (nil? batch-fn)))
  (let [port        (if dev? port 443)
        _           (throw-if-port-in-use! port)
        ctx         (-> (ctx-start)
                      (assoc
                        ::conns (ConcurrentHashMap.))
                      (start-batch-loop!
                        {:dbs           dbs
                         :batch-fn      batch-fn
                         :batch-tick-ms batch-tick-ms}))
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
        config      {:executor (Executors/newVirtualThreadPerTaskExecutor)
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



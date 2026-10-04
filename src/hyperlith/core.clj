(ns hyperlith.core
  (:refer-clojure :exclude [parse-long])
  (:require
   [aleph.http :as http]
   [clojure.main :refer [repl-caught]]
   [hyperlith.core :as-alias h]
   [hyperlith.impl.assets]
   [hyperlith.impl.blocker :refer [wrap-blocker]]
   [hyperlith.impl.cache :as cache]
   [hyperlith.impl.codec :as codec]
   [hyperlith.impl.crypto :as crypto]
   [hyperlith.impl.css]
   [hyperlith.impl.datastar :as ds]
   [hyperlith.impl.env]
   [hyperlith.impl.html]
   [hyperlith.impl.json :refer [wrap-parse-json-body]]
   [hyperlith.impl.lane-context :as lc]
   [hyperlith.impl.namespaces :refer [import-vars]]
   [hyperlith.impl.params :refer [wrap-query-params]]
   [hyperlith.impl.router :as router]
   [hyperlith.impl.session :refer [wrap-session]]
   [hyperlith.impl.sqlite :as sqlite]
   [hyperlith.impl.trace]
   [hyperlith.impl.util :as u]
   [manifold.deferred :as d]
   [manifold.stream :as s]
   [ol.clave.ext.aleph :as clave-aleph])
  (:import
   (manifold.stream.core IEventSink)
   (hyperlith.impl.lane_context LaneCtx)
   (java.net ServerSocket)
   [java.nio ByteBuffer]
   (java.util ArrayList Collection)
   (java.util.concurrent
    ConcurrentHashMap
    ConcurrentLinkedQueue
    CyclicBarrier
    Executors
    LinkedBlockingQueue)))

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
   html->stream
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
  [ctx {:keys [batch-fn batch-tick-ms dbs lanes start-barrier done-barrier
               render-queue conns]}]
  (assert (not (nil? batch-tick-ms)))
  (assert (not (nil? batch-fn)))
  (assert (not (nil? dbs)))
  (assert (not (nil? lanes)))
  (assert (not (nil? start-barrier)))
  (assert (not (nil? done-barrier)))
  (assert (not (nil? render-queue)))
  (let [overruns (atom [])
        q        (LinkedBlockingQueue/new)
        ctx      (merge ctx (sqlite/create-write-connections! dbs))
        t        (Thread.
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
                             (.addAll
                               ^ConcurrentLinkedQueue
                               render-queue
                               ^Collection
                               (.values ^ConcurrentHashMap conns))
                             ;; Start renders
                             (.await ^CyclicBarrier start-barrier)
                             ;; Wait until all renders complete
                             (.await ^CyclicBarrier done-barrier)
                             (catch Throwable t
                               (repl-caught t)
                               (flush)))
                           (let [sleep-time-ms
                                 (- next-tick (System/currentTimeMillis))]
                             (when (< sleep-time-ms 0)
                               ;; log overrun
                               (when (= @overruns [])
                                 (Thread/startVirtualThread
                                   (fn [] (Thread/sleep 20000)
                                     (println "WARNING: tick overrun")
                                     (println (u/stats @overruns))
                                     (reset! overruns []))))
                               (swap! overruns conj
                                 (- batch-tick-ms sleep-time-ms)))
                             (when (> sleep-time-ms 0)
                               (when (not (= @overruns []))
                                 (swap! overruns conj batch-tick-ms))
                               (Thread/sleep ^long sleep-time-ms))))))))
        _        (Thread/.start t)]
    (-> (assoc ctx
          ::tx!
          (fn tx! [thunk] (LinkedBlockingQueue/.offer q thunk)) )
      (update ::stop!
        conj (fn [] (Thread/.interrupt t))))))

(defn- init-render-lanes
  [{:keys [render-pool-size dbs render-buffer-size start-barrier done-barrier
           fragment-cache-size render-queue]}]
  (assert (not (nil? render-pool-size)))
  (assert (not (nil? render-buffer-size)))
  (assert (not (nil? dbs)))
  (assert (not (nil? start-barrier)))
  (assert (not (nil? done-barrier)))
  (assert (not (nil? render-queue)))
  (assert (not (nil? fragment-cache-size)))
  (let [cache (cache/init
                {:max-weight fragment-cache-size
                 :weigher    (fn [_k ^bytes v] (alength v))})]
    (->> (range render-pool-size)
      (mapv (fn [_]
              (let [lane-ctx ^LaneCtx
                    (lc/new-lane-ctx
                      {:dbs            (sqlite/create-read-connections! dbs)
                       :html-dst-buf   (ByteBuffer/allocateDirect
                                       render-buffer-size)
                       :fragment-cache cache})]
                (-> (Thread.
                      ^Runnable
                      (bound-fn* ;; binding conveyance
                        (fn render-thread []
                          (while (not (Thread/interrupted))
                            (.await ^CyclicBarrier start-barrier)
                              (run! sqlite/start-read-tx
                                (vals (.dbs lane-ctx)))
                              (u/while-some
                                [render (.poll
                                          ^ConcurrentLinkedQueue
                                          render-queue)]
                                (render lane-ctx))
                              (run! sqlite/end-read-tx
                                (vals (.dbs lane-ctx)))
                            (.await ^CyclicBarrier done-barrier)))))
                  Thread/.start)
                lane-ctx))))))

(def stub-stream
  (reify IEventSink
    (put      [_ _ _]     (d/success-deferred true))
    (put      [_ _ _ _ _] (d/success-deferred true))
    (isClosed [_] false)
    (markClosed [_] nil)
    (onClosed   [_ _] nil)))

(defn start-app
  [{:keys [port ctx-start batch-fn batch-tick-ms
           domain email mode dbs render-pool-size render-buffer-size
           fragment-cache-size]
    :or   {port                8080
           batch-tick-ms       50
           ctx-start           (fn [] {})
           render-pool-size    (Runtime/.availableProcessors
                                 (Runtime/getRuntime))
           render-buffer-size  (* 32 16384)
           fragment-cache-size (* 512 1024 1024)}}]
  (assert (#{:test :dev :prod} mode))
  (let [port          (if (= mode :prod) 443 port)
        start-barrier (CyclicBarrier/new (inc render-pool-size))
        done-barrier  (CyclicBarrier/new (inc render-pool-size))
        render-queue  (ConcurrentLinkedQueue/new)
        conns         (ConcurrentHashMap/new)
        stream-fn     (if (= mode :test)
                        (fn strem-fn-stub [] stub-stream)
                        (fn stream-fn [] (s/stream 0 nil)))
        lanes         (init-render-lanes
                        {:render-pool-size    render-pool-size
                         :render-buffer-size  render-buffer-size
                         :dbs                 dbs
                         :start-barrier       start-barrier
                         :done-barrier        done-barrier
                         :render-queue        render-queue
                         :fragment-cache-size fragment-cache-size})
        _             (throw-if-port-in-use! port)
        ctx           (-> (ctx-start)
                        (start-batch-loop!
                          {:lanes         lanes
                           :dbs           dbs
                           :batch-fn      batch-fn
                           :batch-tick-ms batch-tick-ms
                           :start-barrier start-barrier
                           :done-barrier  done-barrier
                           :render-queue  render-queue
                           :conns         conns}))
        wrap-ctx      (fn [handler]
                        (fn [req]
                          (handler
                            (-> req
                              (u/fast-merge ctx)
                              (assoc
                                ::h/conns conns
                                ::h/stream-fn stream-fn)))))
        ;; Middleware make for messy error stacks.
        router        (-> router/router
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
        config        {:executor              (Executors/newVirtualThreadPerTaskExecutor)
                       :port                  port
                       ;; Actions payloads are small
                       :max-request-body-size 4096
                       :request-buffer-size   4096}]
    (if (= mode :test)
      {:wrapped-router router
       :ctx            ctx
       :conns          conns}
      (let [server
            (if (not= mode :prod)
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
                             (run! (fn [stop!] (stop!)))))}))))



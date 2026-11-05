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
   [hyperlith.impl.html :as html]
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
   (hyperlith.impl.lane_context LaneCtx)
   (java.net ServerSocket)
   [java.nio ByteBuffer]
   (java.util ArrayList Collection)
   (java.util.concurrent
     ConcurrentHashMap
     CyclicBarrier
     Executors
     LinkedBlockingQueue)
   (java.util.concurrent.atomic AtomicInteger)
   (manifold.stream.core IEventSink)))

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
   html->bytes-oneshot]
  ;; CRYPTO
  [hyperlith.impl.crypto
   new-uid
   digest]
  ;; DATASTAR
  [hyperlith.impl.datastar
   datastar
   datastar-source-map
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
  [sym opts args & body]
  (let [sym-fn (symbol (str sym "-fn"))]
    `(do (defn ~sym-fn ~args ~@body)
         (def ~sym
           (ds/action-handler (var ~sym-fn) ~opts)))))

(defmacro defview
  {:clj-kondo/lint-as 'clojure.core/defn}
  [sym opts args & body]
  (let [sym-fn (symbol (str sym "-fn"))]
    `(do (defn ~sym-fn ~args ~@body)
         (def ~sym
           (ds/view-handler (var ~sym-fn) ~opts)))))

(defmacro defc
  {:clj-kondo/lint-as 'clojure.core/defn}
  [sym args {:req! {k :key}} & body]
  `(defn ~sym ~args
     (html/->Component ~k (fn [] ~@body))))

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

(defn new-app! [{:keys! [batch-fn domain db routes]}]
  (let [q          (LinkedBlockingQueue/new)
        write-conn (sqlite/new-write-conn! db)]
    {:routes         routes
     :domain         domain
     :new-read-conn! (fn [in-component?] (sqlite/new-read-conn! db
                                           in-component?))
     :checkpoint!    (fn checkpoint! []
                       (sqlite/q write-conn
                         {:raw "PRAGMA wal_checkpoint(TRUNCATE)"}))
     :batch!         (fn batch! []
                       (let [batch (ArrayList/new)]
                         (.drainTo q batch)
                         (batch-fn {:db write-conn} (seq batch))))
     :tx!            (fn tx! [thunk] (LinkedBlockingQueue/.offer q thunk))}))

(defn- init-render-lanes
  [{:keys! [render-pool-size apps render-buffer-size
            start-b done-b render-batch render-cursor fragment-cache]}]
  (->> (range render-pool-size)
    (mapv (fn [_]
            (let [in-component? (volatile! nil)
                  lane-ctx      ^LaneCtx
                  (lc/new-lane-ctx
                    {:in-component?  in-component?
                     :dbs            (into {}
                                       (map (fn [app]
                                              [(app :domain)
                                               ((app :new-read-conn!)
                                                in-component?)]))
                                       apps)
                     :html-dst-buf   (ByteBuffer/allocateDirect
                                       render-buffer-size)
                     :fragment-cache fragment-cache})]
              (-> (Thread.
                    ^Runnable
                    (bound-fn* ;; binding conveyance
                      (fn render-thread []
                        (while (not (Thread/interrupted))
                          (.await ^CyclicBarrier start-b)
                          (let [dbs   (vals (.dbs lane-ctx))
                                batch ^java.lang.Object/1 @render-batch
                                n     (alength batch)]
                            (run! sqlite/start-read-tx dbs)
                            (loop []
                              ;; Cool thing here is we can change the step
                              ;; so take 4 or 8 items instead of 1. Renders
                              ;; would have to be sub very fast for this to
                              ;; matter, potentially if you're work skipping.
                              (let [i (AtomicInteger/.getAndIncrement
                                        render-cursor)]
                                (when (< i n)
                                  ((aget batch i) lane-ctx)
                                  (recur))))
                            (run! sqlite/end-read-tx dbs))
                          (.await ^CyclicBarrier done-b)))))
                Thread/.start)
              lane-ctx)))))

(defn start-loop!
  [{:keys! [tick-ms render-pool-size conns
            fragment-cache-size apps]
    :as    m}]
  (let [overruns      (atom [])
        start-b       (CyclicBarrier/new (inc render-pool-size))
        done-b        (CyclicBarrier/new (inc render-pool-size))
        render-batch  (volatile! (object-array 0))
        render-cursor (AtomicInteger. 0)
        cache         (cache/init
                        {:max-weight fragment-cache-size
                         :weigher    (fn [_k ^bytes v] (alength v))
                         ;; Disable Caffeine's background maintenance we will
                         ;; run it manually every tick.
                         :executor
                         (reify java.util.concurrent.Executor
                           (execute [_ _task] nil))})
        ;; Create writers before lanes
        _             (init-render-lanes
                        (assoc m
                          :apps apps
                          :render-batch  render-batch
                          :render-cursor render-cursor
                          :start-b start-b
                          :done-b  done-b
                          :fragment-cache cache))
        t
        (Thread.
          ^Runnable
          (bound-fn* ;; binding conveyance
            (fn loop-thread []
              ;; pipeline ignition
              (.await ^CyclicBarrier start-b)
              (while (not (Thread/interrupted))
                (let [next-tick (+ (System/currentTimeMillis)
                                  tick-ms)]
                  (try
                    (run! (fn [app] ((app :batch!))) apps)
                    (catch Throwable t
                      (repl-caught t)
                      (flush)))
                  ;; Wait until all renders complete
                  (.await ^CyclicBarrier done-b)
                  ;; Cache maintenance while no renders are running
                  (cache/clean-up! cache)
                  ;; Checkpoint while no transactions are running
                  (run! (fn [app] ((app :checkpoint!))) apps)
                  ;; render queue
                  (vreset! render-batch
                    (.toArray ^Collection conns))
                  (AtomicInteger/.set render-cursor 0)
                  ;; Start pipeline (renders + writer)
                  (.await ^CyclicBarrier start-b)
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
                        (- tick-ms sleep-time-ms)))
                    (when (> sleep-time-ms 0)
                      (when (not (= @overruns []))
                        (swap! overruns conj tick-ms))
                      (Thread/sleep ^long sleep-time-ms))))))))
        _             (Thread/.start t)]
    (into {} (map (fn [app] [(app :domain) (app :tx!)])) apps)))

(def stub-stream
  (reify IEventSink
    (put      [_ _ _]     (d/success-deferred true))
    (put      [_ _ _ _ _] (d/success-deferred true))
    (isClosed [_] false)
    (markClosed [_] nil)
    (onClosed   [_ _] nil)))

(defn start-server!
  [{:keys [port tick-ms
           email mode render-pool-size render-buffer-size
           fragment-cache-size apps]
    :or   {port                8080
           tick-ms             50
           render-pool-size    (Runtime/.availableProcessors
                                 (Runtime/getRuntime))
           render-buffer-size  (* 32 16384)
           fragment-cache-size (* 512 1024 1024)}}]
  (assert (#{:test :dev :prod} mode))
  (let [apps      (mapv new-app! apps)
        port      (if (= mode :prod) 443 port)
        conns     (ConcurrentHashMap/newKeySet)
        stream-fn (if (= mode :test)
                    (fn strem-fn-stub [] stub-stream)
                    (fn stream-fn [] (s/stream 0 nil)))
        _         (throw-if-port-in-use! port)
        db-txs!   (start-loop!
                    {:render-pool-size    render-pool-size
                     :render-buffer-size  render-buffer-size
                     :fragment-cache-size fragment-cache-size
                     :apps                apps
                     :tick-ms             tick-ms
                     :conns               conns})
        wrap-ctx  (fn [handler]
                    (fn [req]
                      (handler
                        (assoc req
                          ::h/db-txs! db-txs!
                          ::h/conns conns
                          ::h/stream-fn stream-fn))))
        ;; Middleware make for messy error stacks.
        router    (-> (router/router apps mode)
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
        config    {:executor              (Executors/newVirtualThreadPerTaskExecutor)
                   :port                  port
                   ;; Actions payloads are small
                   :max-request-body-size 4096
                   :request-buffer-size   4096}]
    (case mode
      :test nil
      :prod (clave-aleph/start-server router
              (merge
                config
                {:http-versions             [:http2 :http1]
                 ::clave-aleph/http-options {:port 80}
                 ::clave-aleph/config
                 {:domains (mapv :domain apps)
                  :issuers
                  [{:directory-url
                    "https://acme-v02.api.letsencrypt.org/directory"
                    :email email}]}}))
      :dev  (http/start-server router config))
    {::h/wrapped-router router
     ::h/db-txs!        db-txs!
     ::h/conns          conns}))



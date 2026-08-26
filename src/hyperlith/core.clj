(ns hyperlith.core
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
   [hyperlith.impl.namespaces :refer [import-vars]]
   [hyperlith.impl.params :refer [wrap-query-params]]
   [hyperlith.impl.router :as router]
   [hyperlith.impl.session :refer [wrap-session]]
   [hyperlith.impl.trace]
   [hyperlith.impl.util :as u]
   [hyperlith.impl.sqlite :as sqlite]
   [ol.clave.ext.aleph :as clave-aleph])
  (:import
   (java.net ServerSocket)
   (java.util ArrayList)
   (java.util.concurrent
     TimeUnit
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
   try-parse-long
   modulo-pick]
  ;; HTML
  [hyperlith.impl.html
   html
   html->str
   html-raw-str
   html-resolve-alias]
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

(defn start-batch-loop!
  [{:keys [::conns ::render-pool] :as ctx}
   {:keys [batch-fn batch-tick-ms dbs]}]
  (let [q   (LinkedBlockingQueue/new)
        ctx (merge ctx dbs)
        n   (.getCorePoolSize render-pool)
        t   (Thread/startVirtualThread
              (bound-fn* ;; binding conveyance
                (fn batch-thread []
                  (while (not (Thread/interrupted))
                    (let [next-tick (+ (System/currentTimeMillis) batch-tick-ms)
                          batch     (ArrayList/new)]
                      (.drainTo q batch)
                      (try
                        (batch-fn ctx (seq batch))
                        ;; Refresh connections
                        (let [v (->> (ConcurrentHashMap/.entrySet conns)
                                  (sort-by java.util.Map$Entry/.getKey)
                                  (mapv java.util.Map$Entry/.getValue))]
                          (->> (range n)
                            (mapv (fn [i]
                                    ^java.util.concurrent.Callable
                                    (fn []
                                      (run! sqlite/start-read-tx
                                        (vals sqlite/*dbs*))
                                      (loop [k 0]
                                        (let [idx (+ i (* n k))]
                                          (when (< idx (count v))
                                            ((nth v idx))
                                            (recur (inc k)))))
                                      (run! sqlite/end-read-tx
                                        (vals sqlite/*dbs*)))))
                            (ThreadPoolExecutor/.invokeAll render-pool)))
                        (catch Throwable t
                          (repl-caught t)
                          (flush)))
                      (Thread/sleep ;; sleep 0 to let other tasks run
                        (int (max 0 (- next-tick
                                      (System/currentTimeMillis))))))))))]
    (-> (assoc ctx
          ::tx!
          (fn tx! [thunk] (LinkedBlockingQueue/.offer q thunk)) )
      (update ::stop!
        conj (fn [] (Thread/.interrupt t))))))

(defn- render-thread-factory [dbs]
  (let [base (Executors/defaultThreadFactory)]
    (reify java.util.concurrent.ThreadFactory
      (newThread [_ r]
        (.newThread base
          ;; TODO: Make sqlite connections closable?
          #(binding [sqlite/*dbs* (sqlite/create-read-connections! dbs)]
             (.run ^Runnable r)))))))

(defn start-app
  [{:keys [port ctx-start batch-fn batch-tick-ms
           domain email dev? dbs]
    :or   {port 8080 batch-tick-ms 50 ctx-start (fn [] {})}}]
  (assert (not (nil? batch-fn)))
  (throw-if-port-in-use! 8080)
  (let [ncores   (Runtime/.availableProcessors (Runtime/getRuntime))
        ctx      (-> (ctx-start)
                   (assoc
                     ::conns (ConcurrentHashMap.)
                     ::render-pool
                     (ThreadPoolExecutor.
                       ncores ncores
                       0 TimeUnit/MILLISECONDS
                       (LinkedBlockingQueue.) (render-thread-factory dbs)))
                   (start-batch-loop!
                     {:dbs           (sqlite/create-write-connections! dbs)
                      :batch-fn      batch-fn
                      :batch-tick-ms batch-tick-ms}))
        wrap-ctx (fn [handler]
                   (fn [req]
                     (handler (u/fast-merge req ctx))))
        ;; Middleware make for messy error stacks.
        router   (-> router/router
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
        server
        (if dev?
          (http/start-server router {:port port})
          (clave-aleph/start-server router
            {;; virtual thread executor
             :executor
             (Executors/newVirtualThreadPerTaskExecutor)
             :port                      443
             :http-versions             [:http2 :http1]
             ::clave-aleph/http-options {:port 80}
             ::clave-aleph/config
             {:domains [domain]
              :issuers
              [{:directory-url
                "https://acme-v02.api.letsencrypt.org/directory"
                :email email}]}}))]
    {:wrapped-router router
     :ctx            ctx
     :stop!          (fn stop [& [_opts]]
                       (clave-aleph/stop server)
                       (->> ctx ::stop!
                         (run! (fn [stop!] (stop!)))))}))



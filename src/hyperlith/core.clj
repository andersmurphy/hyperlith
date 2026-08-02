(ns hyperlith.core
  (:require [hyperlith.impl.assets]
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
            [aleph.http :as http])
  (:import (java.net ServerSocket)
           (java.util.concurrent Executors ThreadPoolExecutor
             ConcurrentHashMap)))

;; Make futures use virtual threads
(set-agent-send-executor!
  (Executors/newVirtualThreadPerTaskExecutor))

(set-agent-send-off-executor!
  (Executors/newVirtualThreadPerTaskExecutor))

(import-vars
  ;; ENV
  [hyperlith.impl.env
   env]
  ;; UTIL
  [hyperlith.impl.util
   load-resource
   assoc-if-missing
   assoc-in-if-missing
   qualify-keys
   modulo-pick
   thread
   try-parse-long]
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

(defonce ^ConcurrentHashMap conns (ConcurrentHashMap.))

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

(defonce ^ThreadPoolExecutor render-pool
  (Executors/newFixedThreadPool
    (Runtime/.availableProcessors (Runtime/getRuntime))))

(defn refresh-all! [& _opts]
  (.invokeAll render-pool
    (sort-by System/identityHashCode (.keySet conns))))

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
        (.printStackTrace t)
        (flush)
        {:status 500}))))

(defn start-app
  [{:keys [port ctx-start ctx-stop]
    :or   {port     8080}}]
  (throw-if-port-in-use! 8080)
  (let [ctx            (ctx-start)
        wrap-ctx       (fn [handler]
                         (fn [req]
                           (handler
                             (-> (into {} req)
                               ;; TODO: context should be it's own submap
                               ;; to avoid merge.
                               (assoc :hyperlith.core/conns conns)
                               (u/merge ctx)))))
        ;; Middleware make for messy error stacks.
        wrapped-router (-> router/router
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
        server         (http/start-server wrapped-router
                         {:port port})]
    {:wrapped-router wrapped-router
     :ctx            ctx
     :stop           (fn stop [& [_opts]]
                       (.close server)
                       (ctx-stop ctx))}))

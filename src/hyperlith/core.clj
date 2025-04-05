(ns hyperlith.core
  (:require [hyperlith.impl.namespaces :refer [import-vars]]
            [hyperlith.impl.session :refer [wrap-session]]
            [hyperlith.impl.json :refer [wrap-parse-json-body]]
            [hyperlith.impl.params :refer [wrap-query-params]]
            [hyperlith.impl.datastar :as ds]            
            [hyperlith.impl.util :as u]            
            [hyperlith.impl.error :as er]
            [hyperlith.impl.crypto]
            [hyperlith.impl.css]
            [hyperlith.impl.http]
            [hyperlith.impl.html]
            [hyperlith.impl.router]
            [hyperlith.impl.cache :as cache]
            [hyperlith.impl.assets]
            [hyperlith.impl.trace]
            [hyperlith.impl.env]
            [clojure.core.async :as a]
            [clojure.pprint :as pprint]
            [org.httpkit.server :as hk]
            [hyperlith.impl.codec :as codec]))

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
   thread]
  ;; HTML
  [hyperlith.impl.html
   html]
  ;; CACHE / WORK SHARING
  [hyperlith.impl.cache
   cache]
  ;; CRYPTO
  [hyperlith.impl.crypto
   new-uid
   digest]
  ;; ROUTER
  [hyperlith.impl.router
   router
   wrap-routes]
  ;; DATASTAR
  [hyperlith.impl.datastar
   shim-handler
   signals
   action-handler
   render-handler
   debug-signals-el]
  ;; HTTP
  [hyperlith.impl.http
   get!
   post!
   throw-if-status-not!]
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
  ;; ERROR
  [hyperlith.impl.error
   try-log]
  ;; CODEC
  [hyperlith.impl.codec
   url-query-string
   url-encode])

(defonce ^:private refresh-ch_ (atom nil))
(defonce ^:private app_ (atom nil))

(defn get-app
  "Return app for debugging at the repl."
  []
  @app_)

(defn refresh-all! []
  (when-let [<refresh-ch @refresh-ch_]
    (a/>!! <refresh-ch :refresh-event)))

(defn start-app [{:keys [router port ctx-start ctx-stop csrf-secret
                         max-refresh-ms on-error]
                  :or   {port           8080
                         max-refresh-ms 100
                         on-error       (fn [_ctx {:keys [error]}]
                                          (pprint/pprint error))}}]
  (let [<refresh-ch  (a/chan (a/dropping-buffer 1))
        _            (reset! refresh-ch_ <refresh-ch)        
        ctx          (ctx-start)
        _            (reset! er/on-error_ (partial on-error ctx))
        refresh-mult (-> (ds/throttle <refresh-ch max-refresh-ms)
                       (a/pipe
                         (a/chan 1 ;; Cache is invalidated before refresh.
                           (map (fn [event] (cache/invalidate-cache!) event))))
                       a/mult)
        wrap-ctx     (fn [handler]
                       (fn [req]
                         (handler
                           (-> (assoc req
                                 :hyperlith.core/refresh-mult refresh-mult)
                             (u/merge ctx)))))
        ;; Middleware make for messy error stacks.
        middleware   (-> router
                       wrap-ctx
                       ;; Wrap error here because req params/body/session
                       ;; have been handled (and provide useful context).
                       er/wrap-error
                       ;; The handlers after this point do not throw errors
                       ;; are robust/lenient.
                       wrap-query-params
                       (wrap-session csrf-secret)
                       wrap-parse-json-body)
        stop-server  (hk/run-server middleware {:port port})
        app          {:ctx  ctx
                      :stop (fn stop [& [opts]]
                              (stop-server opts)
                              (ctx-stop ctx)
                              (a/close! <refresh-ch))}]
    (reset! app_ app)
    app))


;; TODO: url-encode / build-query-string
;; Do these exist in ring.codex?

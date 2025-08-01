(ns hyperlith.core
  (:require [hyperlith.impl.namespaces :refer [import-vars]]
            [hyperlith.impl.session :refer [wrap-session]]
            [hyperlith.impl.json :refer [wrap-parse-json-body]]
            [hyperlith.impl.params :refer [wrap-query-params]]
            [hyperlith.impl.blocker :refer [wrap-blocker]]
            [hyperlith.impl.datastar :as ds]
            [hyperlith.impl.util :as u]
            [hyperlith.impl.error :as er]
            [hyperlith.impl.crypto :as crypto]
            [hyperlith.impl.css]
            [hyperlith.impl.http]
            [hyperlith.impl.html :as h]
            [hyperlith.impl.router :as router]
            [hyperlith.impl.assets]
            [hyperlith.impl.trace]
            [hyperlith.impl.env]
            [hyperlith.impl.batch]
            [clojure.core.async :as a]
            [org.httpkit.server :as hk]
            [hyperlith.impl.codec :as codec])
  (:import (java.util.concurrent Executors)))

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
   try-on-error]
  ;; CODEC
  [hyperlith.impl.codec
   url-query-string
   url-encode]
  ;; JSON
  [hyperlith.impl.json
   json->edn
   edn->json]
  ;; BATCH
  [hyperlith.impl.batch
   batch!])

(defonce ^:private refresh-ch_ (atom nil))

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

(defn refresh-all! [& _opts]
  (when-let [<refresh-ch @refresh-ch_]
    (a/>!! <refresh-ch :refresh-event)))

(defn start-app
  [{:keys [port ctx-start ctx-stop csrf-secret
           max-refresh-ms on-error]
    :or   {port           8080
           max-refresh-ms 100
           on-error       er/default-on-error}}]
  (let [<refresh-ch    (a/chan (a/dropping-buffer 1))
        _              (reset! refresh-ch_ <refresh-ch)
        ctx            (ctx-start)
        _              (reset! er/on-error_ on-error)
        refresh-mult   (-> (ds/throttle <refresh-ch max-refresh-ms)
                        a/mult)
        wrap-ctx       (fn [handler]
                        (fn [req]
                          (handler
                            (-> (assoc req
                                  :hyperlith.core/refresh-mult refresh-mult)
                              (u/merge ctx)))))
        ;; Middleware make for messy error stacks.
        wrapped-router (-> router/router
                         wrap-ctx
                         ;; Wrap error here because req params/body/session
                         ;; have been handled (and provide useful context).
                         er/wrap-error
                         ;; The handlers after this point do not throw errors
                         ;; are robust/lenient.
                         wrap-query-params
                         (wrap-session csrf-secret)
                         wrap-parse-json-body
                         wrap-blocker)
        stop-server    (hk/run-server wrapped-router {:port port})]
    {:wrapped-router wrapped-router
     :ctx            ctx
     :stop           (fn stop [& [opts]]
                      (stop-server opts)
                      (ctx-stop ctx)
                      (a/close! <refresh-ch))}))

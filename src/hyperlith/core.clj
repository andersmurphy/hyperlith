(ns hyperlith.core
  (:require [hyperlith.impl.namespaces :refer [import-vars]]
            [hyperlith.impl.session :refer [wrap-session]]
            [hyperlith.impl.json :refer [wrap-parse-json-body]]
            [hyperlith.impl.params :refer [wrap-query-params]]
            [hyperlith.impl.datastar :as ds]
            [hyperlith.impl.crypto]
            [hyperlith.impl.util]
            [hyperlith.impl.css]
            [hyperlith.impl.http]
            [hyperlith.impl.html]
            [hyperlith.impl.router]
            [hyperlith.impl.cache]
            [hyperlith.impl.assets]
            [hyperlith.impl.trace]
            [clojure.core.async :as a]
            [org.httpkit.server :as hk]))

(import-vars
  ;; UTIL
  [hyperlith.impl.util
   resource
   resource->bytes]
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
   router]
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
   post!]
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
   reset-traces!])

(defonce ^:private refresh-ch_ (atom nil))

(defn refresh-all! []
  (when-let [<refresh-ch @refresh-ch_]
    (a/>!! <refresh-ch :refresh-event)))

(defn start-app [{:keys [router port db-start db-stop csrf-secret
                         max-refresh-ms]}]
  (let [<refresh-ch  (a/chan (a/dropping-buffer 1))
        _            (reset! refresh-ch_ <refresh-ch)
        db           (db-start)
        refresh-mult (ds/throttled-mult <refresh-ch (or max-refresh-ms 100))
        wrap-state   (fn [handler] (fn [req]
                                     (handler
                                       (assoc req :db db
                                         :refresh-mult refresh-mult))))
        stop-server  (-> router
                       wrap-state
                       wrap-query-params
                       (wrap-session csrf-secret)
                       wrap-parse-json-body
                       (hk/run-server {:port (or port 8080)}))]
    {:db   db
     :stop (fn stop []
             (stop-server)
             (db-stop db)
             (a/close! <refresh-ch))}))



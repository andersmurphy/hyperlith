(ns hyperlith.core
  (:require [hyperlith.impl.namespaces :refer [import-vars]]
            [hyperlith.impl.session :refer [wrap-session]]
            [hyperlith.impl.json :refer [wrap-parse-json-body]]
            [hyperlith.impl.params :refer [wrap-query-params]]
            [hyperlith.impl.datastar :as ds]            
            [hyperlith.impl.util :as u]
            [hyperlith.impl.crypto]
            [hyperlith.impl.css]
            [hyperlith.impl.http]
            [hyperlith.impl.html]
            [hyperlith.impl.router]
            [hyperlith.impl.cache :as cache]
            [hyperlith.impl.assets]
            [hyperlith.impl.trace]
            [hyperlith.impl.env]
            [hyperlith.impl.tuples]
            [clojure.core.async :as a]
            [org.httpkit.server :as hk]))

(import-vars
  ;; ENV
  [hyperlith.impl.env
   env]
  ;; UTIL
  [hyperlith.impl.util
   load-resource
   assoc-if-missing
   assoc-in-if-missing]
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
  ;; TUPLES
  [hyperlith.impl.tuples
   tuples]
  ;; TRACE
  [hyperlith.impl.trace
   traces
   trace>
   reset-traces!])

(defonce ^:private refresh-ch_ (atom nil))

(defn refresh-all! []
  (when-let [<refresh-ch @refresh-ch_]
    (a/>!! <refresh-ch :refresh-event)))

(defn start-app [{:keys [router port ctx-start ctx-stop csrf-secret
                         max-refresh-ms]
                  :or   {port           8080
                         max-refresh-ms 100}}]
  (let [<refresh-ch  (a/chan (a/dropping-buffer 1))
        _            (reset! refresh-ch_ <refresh-ch)
        ctx          (ctx-start)
        refresh-mult (-> (ds/throttle <refresh-ch max-refresh-ms)
                       (a/pipe
                         (a/chan 1 ;; cache is invalidated before refresh
                           (map (fn [event] (cache/invalidate-cache!) event))))
                       a/mult)
        wrap-ctx     (fn [handler]
                     (fn [req]
                       (handler
                         (-> (assoc req
                               :hyperlith.core/refresh-mult refresh-mult)
                           (u/merge ctx)))))
        stop-server  (-> router
                       wrap-ctx
                       wrap-query-params
                       (wrap-session csrf-secret)
                       wrap-parse-json-body
                       (hk/run-server {:port port}))]
    {:ctx  ctx
     :stop (fn stop []
             (stop-server)
             (ctx-stop ctx)
             (a/close! <refresh-ch))}))

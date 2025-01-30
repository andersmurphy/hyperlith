(ns hyperlith.core
  (:require [hyperlith.impl.session :refer [wrap-session]]
            [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.json :refer [wrap-parse-json-body]]
            [hyperlith.impl.datastar :as ds]
            [hiccup2.core :as h]
            [org.httpkit.server :as hk]
            [clojure.core.async :as a]))

(defmacro thread-while-some
  {:clj-kondo/lint-as 'clojure.core/let}
  [bindings & body]
  `(Thread/startVirtualThread
     #(loop []
        (when-some ~bindings
          ~@body
          (recur)))))

(defn- throttled-mult [<in-ch msec]
  (let [<out-ch (a/chan (a/dropping-buffer 1))]
    (thread-while-some [v (a/<!! <in-ch)]
      (a/>!! <out-ch v)
      (Thread/sleep ^long msec))
    (a/mult <out-ch)))

(defn- send! [ch event close-after-send?]
  (hk/send! ch {:status  200
                :headers (assoc default-headers
                           "Content-Type"  "text/event-stream"
                           "Cache-Control" "no-store")
                :body    event}
    close-after-send?))

(defn new-uid
  "Allows us to change id implementation if need be."
  []
  (str (random-uuid)))

;; HTML
(defmacro html-str [hiccup]
  `(-> (h/html ~hiccup) str))

;; ROUTING
(defn router
  ([routes] (router routes (fn [_] {:status 303 :headers {"Location" "/"}})))
  ([routes not-found-handler]
   (let [routes (merge ds/default-routes routes)]
     (fn [req]
       ((routes [(:request-method req) (:uri req)] not-found-handler) req)))))

;; HANDLERS
(defn shim-handler [opts]
  (let [resp (ds/build-shim-page-resp opts)]
    (fn handler [_req] resp)))

(defn action-handler [thunk]
  (fn handler [req]
    (thunk req)
    {:status  204
     :headers default-headers}))

(defn render-handler [render-fn]
  (fn handler [req]
    (let [<ch (a/tap (:refresh-mult req) (a/chan (a/dropping-buffer 1)))]
      (hk/as-channel req
        {:on-open
         (fn on-open [ch]
           ;; Get latest render on connect/reconnect
           (send! ch (ds/merge-fragments (render-fn req)) false)
           ;; Subscribe to subsequent refreshes
           (thread-while-some [_ (a/<!! <ch)]
             (send! ch (ds/merge-fragments (render-fn req)) false)))
         :on-close (fn on-close [_ _] (a/close! <ch))}))))

(defonce ^:private refresh-ch_ (atom nil))

(defn refresh-all! []
  (a/>!! @refresh-ch_ :refresh-event))

;; APP
(defn start-app [{:keys [router port db-start db-stop csrf-secret
                         max-refresh-ms]}]
  (let [<refresh-ch  (a/chan (a/dropping-buffer 1))
        _            (reset! refresh-ch_ <refresh-ch)
        db           (db-start)
        refresh-mult (throttled-mult <refresh-ch (or max-refresh-ms 100))
        wrap-state   (fn [handler] (fn [req]
                                     (handler
                                       (assoc req :db db
                                         :refresh-mult refresh-mult))))
        stop-server  (-> router
                       wrap-state
                       (wrap-session csrf-secret)
                       wrap-parse-json-body
                       (hk/run-server {:port (or port 8080)}))]
    {:db   db
     :stop (fn stop []
             (stop-server)
             (db-stop db)
             (a/close! <refresh-ch))}))

;; Compress SSE stream
;; solve CSS

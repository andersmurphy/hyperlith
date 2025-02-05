(ns hyperlith.core
  (:require [hyperlith.impl.session :refer [wrap-session]]
            [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.assets :as assets]
            [hyperlith.impl.css :as css]
            [hyperlith.impl.json :refer [wrap-parse-json-body]]
            [hyperlith.impl.gzip :as gz]
            [hyperlith.impl.datastar :as ds]
            [hiccup2.core :as h]
            [org.httpkit.server :as hk]
            [clojure.core.async :as a]))

(defmacro thread [& body]
  `(Thread/startVirtualThread
     (fn [] ~@body)))

(defmacro while-some
  {:clj-kondo/lint-as 'clojure.core/let}
  [bindings & body]
  `(loop []
     (when-some ~bindings
       ~@body
       (recur))))

(defonce ^:private tick_ (atom (bigint 0)))
(def ^:private  do-not-refresh-shared-queries
  "By being smaller than any tick, this tick will never trigger
  a refresh of shared queries."
  (bigint -1))

(defn- throttled-mult [<in-ch msec]
  (let [;; No buffer on the out-ch as the in-ch should be buffered
        <out-ch (a/chan)]
    (thread
      (while-some [_ (a/<!! <in-ch)]
        ;; Each mult event is a sequentially increasing value
        (a/>!! <out-ch (swap! tick_ inc))
        (Thread/sleep ^long msec)))
    (a/mult <out-ch)))

(defn- send! [ch event]
  (hk/send! ch {:status  200
                :headers (assoc default-headers
                           "Content-Type"  "text/event-stream"
                           "Cache-Control" "no-store"
                           "Content-Encoding" "gzip")
                :body    event}
    false))

(defn new-uid
  "Allows us to change id implementation if need be."
  []
  (str (random-uuid)))

;; HTML
(defmacro html [& hiccups]
  `(str ~@(map (fn [hiccup] `(h/html ~hiccup)) hiccups)))

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
    (let [;; Dropping buffer is used here as we don't want a slow handler
          ;; blocking other handlers. Mult distributes each event to all
          ;; taps in parallel and synchronously, i.e. each tap must
          ;; accept before the next item is distributed.
          <ch     (a/tap (:refresh-mult req) (a/chan (a/dropping-buffer 1)))
          ;; Ensures at least one render on connect
          _       (a/>!! <ch do-not-refresh-shared-queries)
          ;; poison pill for work cancelling
          <cancel (a/chan)]
      (hk/as-channel req
        {:on-open
         (fn on-open [ch]
           (thread
             ;; Note: it is possible to perform diffing here. However, because
             ;; we are gziping the stream for the duration of the connection
             ;; and html compresses well we get insane compression. To the
             ;; point were it's more network efficient and more performant
             ;; than diffing.
             (with-open [out  (gz/byte-array-out-stream)
                         gzip (gz/gzip-out-stream out)]
               (loop [last-view-hash nil]
                 (a/alt!!
                   [<cancel] (do (a/close! <ch) (a/close! <cancel))
                   [<ch]     (let [new-view      (render-fn req)
                                     new-view-hash (hash new-view)]
                                 ;; only send an event if the view has changed
                                 (when (not= last-view-hash new-view-hash)
                                   (->> new-view ds/merge-fragments
                                     (gz/gzip-chunk out gzip)
                                     (send! ch)))
                                 (recur new-view-hash))
                   ;; we want work cancelling to have higher priority
                   :priority true)))))
         :on-close (fn on-close [_ _] (a/>!! <cancel :cancel))}))))

(def static-asset assets/static-asset)

(def static-css css/static-css)

(defonce ^:private refresh-ch_ (atom nil))

(defn refresh-all! []
  (when-let [<refresh-ch @refresh-ch_]
    (a/>!! <refresh-ch :refresh)))

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

;; solve CSS
;; check animations


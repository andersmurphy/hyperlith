(ns hyperlith.core
  (:require [hyperlith.impl.session :refer [wrap-session csrf-cookie-js]]
            [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.assets :as assets]
            [hyperlith.impl.css :as css]
            [hyperlith.impl.json :refer [wrap-parse-json-body]]
            [hyperlith.impl.params :refer [wrap-query-params]]
            [hyperlith.impl.gzip :as gz]
            [hyperlith.impl.datastar :as ds]
            [hyperlith.impl.crypto :as crypto]
            [hiccup2.core :as h]
            [org.httpkit.server :as hk]
            [clojure.core.async :as a]
            [clojure.java.io :as io])
  (:import (java.io InputStream)))

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

(def new-uid
  "Allows us to change id implementation if need be."
  crypto/random-unguessable-uid)

(def digest
  "Digest function based on Clojure's hash."
  crypto/digest)

(defmacro resource
  "Fails at compile time if resource doesn't exists."
  [path]
  (let [res (io/resource path)]
    (assert res (str path " not found."))
    `(io/resource ~path)))

(defn resource->bytes [resource]
  (-> resource io/input-stream InputStream/.readAllBytes))

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

(defn- build-shim-page-resp [head-html]
  {:status  200
   :headers (assoc default-headers "Content-Encoding" "gzip")
   :body
   (->> (h/html
          [:html  {:lang "en"}
           [:head
            [:meta {:charset "UTF-8"}]
            (when head-html (h/raw head-html))
            ;; Scripts
            [:script#js {:defer true :type "module"
                         :src   (ds/datastar :path)}]
            ;; Enables responsiveness on mobile devices
            [:meta {:name    "viewport"
                    :content "width=device-width, initial-scale=1.0"}]]
           [:body
            [:div {:data-signals-csrf csrf-cookie-js}]
            [:div {:data-on-load
                   "@post(window.location.pathname.replace(/\\/$/,'') + '/updates' + window.location.search)"}]
            [:noscript "Your browser does not support JavaScript!"]
            [:main {:id "morph"}]]])
     (str "<!DOCTYPE html>")
     gz/gzip)})

;; HTML
(defmacro html [& hiccups]
  `(str ~@(map (fn [hiccup] `(h/html ~hiccup)) hiccups)))

;; ROUTING
(defn router
  ([routes] (router routes (fn [_] {:status 404})))
  ([routes not-found-handler]
   (let [routes (merge ds/routes routes)]
     (fn [req]
       ((routes [(:request-method req) (:uri req)] not-found-handler) req)))))

;; HANDLERS
(defn shim-handler [head-hiccup]
  (let [resp (build-shim-page-resp head-hiccup)]
    (fn handler [_req] resp)))

(defn action-handler [thunk]
  (fn handler [req]
    (thunk req)
    {:status  204
     :headers default-headers}))

(defn render-handler [render-fn & {:keys [on-close on-open] :as _opts}]
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
         (fn hk-on-open [ch]
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
                   :priority true))))
           (when on-open (on-open req)))
         :on-close (fn hk-on-close [_ _]
                     (a/>!! <cancel :cancel)
                     (when on-close (on-close req)))}))))

;; ASSETS
(def static-asset assets/static-asset)

(def static-css css/static-css)

(def -- css/--)

;; REFRESH
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
                       wrap-query-params
                       (wrap-session csrf-secret)
                       wrap-parse-json-body
                       (hk/run-server {:port (or port 8080)}))]
    {:db   db
     :stop (fn stop []
             (stop-server)
             (db-stop db)
             (a/close! <refresh-ch))}))

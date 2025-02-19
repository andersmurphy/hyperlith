(ns hyperlith.impl.datastar
  (:require [hyperlith.impl.assets :refer [static-asset]]
            [hyperlith.impl.session :refer [csrf-cookie-js]]
            [hyperlith.impl.json :as j]
            [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.util :as util]
            [hyperlith.impl.cache :as cache]            
            [hyperlith.impl.gzip :as gz]            
            [hyperlith.impl.crypto :as crypto]                        
            [hyperlith.impl.html :as h]
            [org.httpkit.server :as hk]
            [clojure.string :as str]
            [clojure.core.async :as a]))

(def datastar-source-map
  (static-asset
    {:body         (util/load-resource "datastar.js.map")
     :content-type "text/javascript"
     :gzip?        true}))

(def datastar
  (static-asset
    {:body
     (-> (util/load-resource "datastar.js") slurp
       ;; Make sure we point to the right source map
       (str/replace "datastar.js.map" (:path datastar-source-map)))
     :content-type "text/javascript"
     :gzip?        true}))

(defn merge-fragments [event-id fragments]
  (str "event: datastar-merge-fragments"
    "\nid: " event-id
    "\ndata: fragments " (str/replace fragments "\n" "\ndata: fragments ")
    "\n\n\n"))

(defn merge-signals [signals]
  (str "event: datastar-merge-signals"
    "\ndata: onlyIfMissing false"
    "\ndata: signals " (j/edn->json signals)
    "\n\n\n"))

(defn throttled-mult [<in-ch msec]
  (let [;; No buffer on the out-ch as the in-ch should be buffered
        <out-ch (a/chan)]
    (util/thread
      (util/while-some [_ (a/<!! <in-ch)]
        ;; cache is only invalidate at most every X msec and only if
        ;; db has change
        (cache/invalidate-cache!) 
        (a/>!! <out-ch :refresh)
        (Thread/sleep ^long msec)))
    (a/mult <out-ch)))

(defn send! [ch event]
  (hk/send! ch {:status  200
                :headers (assoc default-headers
                           "Content-Type"  "text/event-stream"
                           "Cache-Control" "no-store"
                           "Content-Encoding" "gzip")
                :body    event}
    false))

(defn build-shim-page-resp [head-html]
  {:status  200
   :headers (assoc default-headers "Content-Encoding" "gzip")
   :body
   (->> (h/html
          [h/doctype-html5
           [:html  {:lang "en"}
           [:head
            [:meta {:charset "UTF-8"}]
            (when head-html (h/raw head-html))
            ;; Scripts
            [:script#js {:defer true :type "module"
                         :src   (datastar :path)}]
            ;; Enables responsiveness on mobile devices
            [:meta {:name    "viewport"
                    :content "width=device-width, initial-scale=1.0"}]]
           [:body
            [:div {:data-signals-csrf csrf-cookie-js}]
            [:div {:data-on-load
                   "@post(window.location.pathname.replace(/\\/$/,'') + '/updates' + window.location.search)"}]
            [:noscript "Your browser does not support JavaScript!"]
            [:main {:id "morph"}]]]])
     h/html->str
     gz/gzip)})

(def routes
  {[:get (datastar :path)]            (datastar :handler)
   [:get (datastar-source-map :path)] (datastar-source-map :handler)})

(defn shim-handler [head-hiccup]
  (let [resp (build-shim-page-resp head-hiccup)]
    (fn handler [_req] resp)))

(defn signals [signals]
  {::signals signals})

(defn action-handler [thunk]
  (fn handler [req]
    (if-let [signals (::signals (thunk req))]
      {:status  200
       :headers (assoc default-headers
                  "Content-Type"  "text/event-stream"
                  "Cache-Control" "no-store"
                  "Content-Encoding" "gzip")
       :body    (gz/gzip (merge-signals signals))}
      {:status  204
       :headers default-headers})))

(defn render-handler [render-fn & {:keys [on-close on-open] :as _opts}]
  (fn handler [req]
    (let [;; Dropping buffer is used here as we don't want a slow handler
          ;; blocking other handlers. Mult distributes each event to all
          ;; taps in parallel and synchronously, i.e. each tap must
          ;; accept before the next item is distributed.
          <ch     (a/tap (:refresh-mult req) (a/chan (a/dropping-buffer 1)))
          ;; Ensures at least one render on connect
          _       (a/>!! <ch :refresh-event)
          ;; poison pill for work cancelling
          <cancel (a/chan)]
      (hk/as-channel req
        {:on-open
         (fn hk-on-open [ch]
           (util/thread
             ;; Note: it is possible to perform diffing here. However, because
             ;; we are gziping the stream for the duration of the connection
             ;; and html compresses well we get insane compression. To the
             ;; point were it's more network efficient and more performant
             ;; than diffing.
             (with-open [out  (gz/byte-array-out-stream)
                         gzip (gz/gzip-out-stream out)]
               (loop [last-view-hash (get-in req [:headers "last-event-id"])]
                 (a/alt!!
                   [<cancel] (do (a/close! <ch) (a/close! <cancel))
                   [<ch]     (let [new-view      (render-fn req)
                                   new-view-hash (crypto/digest new-view)]
                               ;; only send an event if the view has changed
                               (when (not= last-view-hash new-view-hash)
                                 (->> (merge-fragments
                                        new-view-hash (h/html->str new-view))
                                   (gz/gzip-chunk out gzip)
                                   (send! ch)))
                               (recur new-view-hash))
                   ;; we want work cancelling to have higher priority
                   :priority true))))
           (when on-open (on-open req)))
         :on-close (fn hk-on-close [_ _]
                     (a/>!! <cancel :cancel)
                     (when on-close (on-close req)))}))))

(def debug-signals-el [:pre {:data-text "ctx.signals.JSON()"}])

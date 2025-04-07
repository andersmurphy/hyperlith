(ns hyperlith.impl.datastar
  (:require [hyperlith.impl.assets :refer [static-asset]]
            [hyperlith.impl.session :refer [csrf-cookie-js]]
            [hyperlith.impl.json :as j]
            [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.util :as util]
            [hyperlith.impl.brotli :as br]
            [hyperlith.impl.crypto :as crypto]
            [hyperlith.impl.html :as h]
            [hyperlith.impl.error :as er]
            [org.httpkit.server :as hk]
            [clojure.string :as str]
            [clojure.core.async :as a]))

(def datastar-source-map
  (static-asset
    {:body         (util/load-resource "datastar.js.map")
     :content-type "text/javascript"
     :compress?        true}))

(def datastar
  (static-asset
    {:body
     (-> (util/load-resource "datastar.js") slurp
       ;; Make sure we point to the right source map
       (str/replace "datastar.js.map" (:path datastar-source-map)))
     :content-type "text/javascript"
     :compress?        true}))

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

(defn throttle [<in-ch msec]
  (let [;; No buffer on the out-ch as the in-ch should be buffered
        <out-ch (a/chan)]
    (util/thread
      (util/while-some [event (a/<!! <in-ch)]
        (a/>!! <out-ch event)
        (Thread/sleep ^long msec)))
    <out-ch))

(defn send! [ch event]
  (hk/send! ch {:status  200
                :headers (assoc default-headers
                           "Content-Type"  "text/event-stream"
                           "Cache-Control" "no-store"
                           "Content-Encoding" "br")
                :body    event}
    false))

(def on-load-js
  ;; Quirk with browsers is that cache settings are per URL not per
  ;; URL + METHOD this means that GET and POST cache headers can
  ;; mess with each other. To get around this an unused query param
  ;; is added to the url.
  "@post(window.location.pathname + (window.location.search + '&u=').replace(/^&/,'?'))")

(defn build-shim-page-resp [head-hiccup]
  (let [body (-> (h/html
                   [h/doctype-html5
                    [:html  {:lang "en"}
                     [:head
                      [:meta {:charset "UTF-8"}]
                      (when head-hiccup head-hiccup)
                      ;; Scripts
                      [:script#js {:defer true :type "module"
                                   :src   (datastar :path)}]
                      ;; Enables responsiveness on mobile devices
                      [:meta {:name    "viewport"
                              :content "width=device-width, initial-scale=1.0"}]]
                     [:body
                      [:div {:data-signals-csrf csrf-cookie-js}]
                      [:div {:data-on-load on-load-js}]
                      [:noscript "Your browser does not support JavaScript!"]
                      [:main {:id "morph"}]]]])
               h/html->str)]
    (-> {:status  200
         :headers (assoc default-headers "Content-Encoding" "br")
         :body    (-> body (br/compress :quality 11))}
      ;; Etags ensure the shim is only sent again if it's contents have changed
      (assoc-in [:headers "ETag"] (crypto/digest body)))))

(def routes
  {[:get (datastar :path)]            (datastar :handler)
   [:get (datastar-source-map :path)] (datastar-source-map :handler)})

(defn shim-handler [head-hiccup]
  (let [resp (build-shim-page-resp head-hiccup)
        etag (get-in resp [:headers "ETag"])]    
    (fn handler [req]
      (if (= (get-in req [:headers "if-none-match"]) etag)
        {:status 304}
        resp))))

(defn signals [signals]
  {:hyperlith.core/signals signals})

(defn action-handler [thunk]
  (fn handler [req]
    (if-let [signals (:hyperlith.core/signals (thunk req))]
      {:status  200
       :headers (assoc default-headers
                  "Content-Type"  "text/event-stream"
                  "Cache-Control" "no-store"
                  "Content-Encoding" "br")
       :body    (br/compress (merge-signals signals))}
      {:status  204
       :headers default-headers})))

(defn render-handler [render-fn & {:keys [on-close on-open] :as _opts}]
  (fn handler [req]
    (let [;; Dropping buffer is used here as we don't want a slow handler
          ;; blocking other handlers. Mult distributes each event to all
          ;; taps in parallel and synchronously, i.e. each tap must
          ;; accept before the next item is distributed.
          <ch     (a/tap (:hyperlith.core/refresh-mult req)
                    (a/chan (a/dropping-buffer 1)))
          ;; Ensures at least one render on connect
          _       (a/>!! <ch :refresh-event)
          ;; poison pill for work cancelling
          <cancel (a/chan)]
      (hk/as-channel req
        {:on-open
         (fn hk-on-open [ch]
           (util/thread
             ;; Note: it is possible to perform diffing here. However, because
             ;; we are compressing the stream for the duration of the
             ;; connection and html compresses well we get insane compression.
             ;; To the point were it's more network efficient and more
             ;; performant than diffing.
             (with-open [out (br/byte-array-out-stream)
                         br  (br/compress-out-stream out
                               ;; Window size can be tuned to trade bandwidth
                               ;; for memory. 262KB is 8x of gzip's 32KB.
                               ;; (br/window-size->kb 18) => 262KB
                               :window-size 18)]
               (loop [last-view-hash (get-in req [:headers "last-event-id"])]
                 (a/alt!!
                   [<cancel] (do (a/close! <ch) (a/close! <cancel))
                   [<ch]     (when-some ;; stop in case of error
                                 [new-view (er/try-log req (render-fn req))]
                               (let [new-view-hash (crypto/digest new-view)]
                                 ;; only send an event if the view has changed
                                 (when (not= last-view-hash new-view-hash)
                                   (->> (merge-fragments
                                          new-view-hash (h/html->str new-view))
                                     (br/compress-stream out br)
                                     (send! ch)))
                                 (recur new-view-hash)))
                   ;; we want work cancelling to have higher priority
                   :priority true))
               ;; Close channel on error or when thread stops
               (hk/close ch)))
           (when on-open (on-open req)))
         :on-close (fn hk-on-close [_ _]
                     (a/>!! <cancel :cancel)
                     (when on-close (on-close req)))}))))

(def debug-signals-el [:pre {:data-text "ctx.signals.JSON()"}])

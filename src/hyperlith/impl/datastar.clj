(ns hyperlith.impl.datastar
  (:require [hyperlith.impl.assets :refer [static-asset]]
            [hyperlith.impl.session :refer [csrf-cookie-js]]
            [hyperlith.impl.headers
             :refer [default-headers strict-transport]]
            [hyperlith.impl.util :as util]
            [hyperlith.impl.brotli :as br]
            [hyperlith.impl.crypto :as crypto]
            [hyperlith.impl.html :as h]
            [hyperlith.impl.error :as er]
            [hyperlith.impl.json :as json]
            [hyperlith.impl.router :as router]
            [hyperlith.impl.cpu-pool :as cp]
            [org.httpkit.server :as hk]
            [clojure.string :as str]
            [clojure.core.async :as a]))

(def datastar-source-map
  (static-asset
    {:body         (util/load-resource "datastar.js.map")
     :content-type "text/javascript"
     :compress?    true}))

(def datastar
  (static-asset
    {:body
     (-> (util/load-resource "datastar.js") slurp
       ;; Make sure we point to the right source map
       (str/replace "datastar.js.map" datastar-source-map))
     :content-type "text/javascript"
     :compress?    true}))

(defn patch-elements [event-id elements]
  (str "event: datastar-patch-elements"
    "\nid: " event-id
    "\ndata: elements " (str/replace elements "\n" "\ndata: elements ")
    "\n\n\n"))

(defn patch-append-body [elements]
  (str "event: datastar-patch-elements"
    "\ndata: selector body"
    "\ndata: mode append"
    "\ndata: elements " (str/replace elements "\n" "\ndata: elements ")
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

  ;; Retry Infinity means we always try to reconnect. The other defaults
  ;; mean that this will at most take 30s (default max backoff).
  "@post(window.location.pathname + (window.location.search + '&u=').replace(/^&/,'?'), {retryMaxCount: Infinity})")

(def tabid-js
  ;; Higher collision risk is acceptable here as it only needs to be
  ;; unique against a given users other tabs.
  "self.crypto.randomUUID().substring(0,8)")

(defn build-shim-page-resp [head-hiccup]
  (let [body (-> (h/html
                   [h/doctype-html5
                    [:html  {:lang "en"}
                     [:head
                      [:meta {:charset "UTF-8"}]
                      (when head-hiccup head-hiccup)
                      ;; Scripts
                      [:script#js {:defer true :type "module"
                                   :src   datastar}]
                      ;; Enables responsiveness on mobile devices
                      [:meta {:name    "viewport"
                              :content "width=device-width, initial-scale=1.0"}]]
                     [:body
                      [:div {:data-signals-csrf  csrf-cookie-js
                             :data-signals-tabid tabid-js}]
                      [:div {:data-on-load           on-load-js
                             ;; Reconnect when the user comes online after
                             ;; being offline. Closes any existing connection
                             ;; from this div.
                             :data-on-online__window on-load-js}]
                      [:noscript "Your browser does not support JavaScript!"]
                      [:main {:id "morph"}]]]])
               h/html->str)]
    (-> {:status  200
         :headers (assoc default-headers "Content-Encoding" "br")
         :body    (-> body (br/compress :quality 11))}
      ;; Etags ensure the shim is only sent again if it's contents have changed
      (assoc-in [:headers "ETag"] (crypto/digest body)))))

(defn shim-handler [path head-hiccup]
  (router/add-route! [:get path]
    (let [resp (build-shim-page-resp head-hiccup)
          etag ((:headers resp) "ETag")]
      (fn handler [req]
        (if (= ((:headers req) "if-none-match") etag)
          {:status 304}
          resp)))))

(defn action-handler [path thunk]
  (router/add-route! [:post path]
    (fn handler [req]
      (let [resp (thunk req)]
        (if (h/chassis-data? resp)
          {:status  200
           :headers {"Content-Type"              "text/event-stream"
                     "Cache-Control"             "no-store"
                     "Content-Encoding"          "br"
                     "Strict-Transport-Security" strict-transport}
           :body    (-> (h/html->str resp)
                      patch-append-body
                      br/compress)}
          ;; 204 needs even less
          {:headers {"Strict-Transport-Security" strict-transport
                     "Cache-Control"             "no-store"}
           :status  204})))))

(defn render-handler
  [path render-fn & {:keys                     [on-close on-open br-window-size
                            render-on-connect] :as _opts
                     :or                       {;; Window size can be tuned to trade memory
                                                ;; for reduced bandwidth and compute.
                                                ;; The right window size can significantly improve
                                                ;; compression of highly variable streams of data.
                                                ;; (br/window-size->kb 18) => 262KB
                                                br-window-size    18
                                                ;; If false does not render on connect  waits for
                                                ;; next batch. Note this means you should do
                                                ;; something on connect to trigger a batch.
                                                ;; Otherwise the user will not see anything
                                                ;; until a batch is triggered.
                                                render-on-connect true}}]
  (router/add-route! [:post path]
    (fn handler [req]
      (let [;; Dropping buffer is used here as we don't want a slow handler
            ;; blocking other handlers. Mult distributes each event to all
            ;; taps in parallel and synchronously, i.e. each tap must
            ;; accept before the next item is distributed.
            <ch     (a/tap (:hyperlith.core/refresh-mult req)
                      (a/chan (a/dropping-buffer 1)))
            ;; Ensures at least one render on connect when enabled
            _       (when render-on-connect (a/>!! <ch :first-render))
            ;; poison pill for work cancelling
            <cancel (a/chan)]
        (hk/as-channel req
          {:on-open
           (fn hk-on-open [ch]
             (util/thread
               (with-open [out (br/byte-array-out-stream)
                           br  (br/compress-out-stream out
                                 :window-size br-window-size)]
                 (loop [last-view-hash ((:headers req) "last-event-id")]
                   (a/alt!!
                     [<cancel]
                     (do (a/close! <ch) (a/close! <cancel))

                     [<ch]
                     ([_]
                      (some-> ;; stop in case of error
                        (cp/on-cpu-pool ;; CPU work on real threads
                          (when-some ;; stop in case of error
                              [new-view (er/try-on-error (render-fn req))]
                            (let [new-view-str  (h/html->str new-view)
                                  ;; This is a very fast hash
                                  new-view-hash (Integer/toHexString
                                                  (hash new-view-str))]
                              ;; only send an event if the view has changed
                              (when (not= last-view-hash new-view-hash)
                                (->> (patch-elements
                                       new-view-hash new-view-str)
                                  (br/compress-stream out br)
                                  (send! ch)))
                              new-view-hash)))
                        recur))
                     ;; we want work cancelling to have higher priority
                     :priority true))
                 ;; Close channel on error or when thread stops
                 (hk/close ch)))
             (when on-open (on-open req)))
           :on-close (fn hk-on-close [_ _]
                       (a/>!! <cancel :cancel)
                       (when on-close (on-close req)))})))))

(defn patch-signals [signals]
  (h/html [:div {:data-signals (json/edn->json signals)
                 :data-on-load "el.remove()"}]))

(defn execute-expr [expr]
  (h/html [:div {:data-on-load (str expr ";el.remove()")}]))


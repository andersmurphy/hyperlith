(ns hyperlith.impl.datastar
  (:require [hyperlith.impl.assets :refer [static-asset]]
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
            [clojure.core.async :as a]
            [clojure.string :as str]))

(def packetstar
  (static-asset
    {:body
     (-> (util/load-resource "packetstar.js") slurp)
     :content-type "text/javascript"
     :compress?    true}))

(defn patch-elements [event-id elements]
  (str "event: message"
    "\nid: " event-id
    "\ndata: " elements
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

(defn build-shim-page-resp [head-hiccup]
  (let [body (->
               (h/html
                 [h/doctype-html5
                  [:html  {:lang "en"}
                   [:head
                    [:meta {:charset "UTF-8"}]
                    (when head-hiccup head-hiccup)
                    ;; Scripts
                    [:script#js {:defer true :type "module"
                                 :src   packetstar}]
                    ;; Enables responsiveness on mobile devices
                    [:meta {:name    "viewport"
                            :content "width=device-width, initial-scale=1.0"}]]
                   [:body
                    [:noscript "Your browser does not support JavaScript!"]]]])
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
      (thunk req)
      {:headers {"Strict-Transport-Security" strict-transport
                 "Cache-Control"             "no-store"}
       :status  204})))

(defn render-handler
  [path render-fn &
   {:keys [on-close on-open br-window-size render-on-connect] :as _opts
    :or   {;; Window size can be tuned to trade memory
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
  (router/add-route! [:get (str/replace (str path "/stream") "//" "/")]
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
                 :data-init    "el.remove()"}]))

(defn execute-expr [expr]
  (h/html [:div {:data-init (str expr ";el.remove()")}]))


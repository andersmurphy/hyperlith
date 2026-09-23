(ns hyperlith.impl.datastar
  (:require
   [clojure.main :refer [repl-caught]]
   [clojure.string :as str]
   [hyperlith.impl.assets :refer [static-asset]]
   [hyperlith.impl.crypto :as crypto]
   [hyperlith.impl.headers
    :refer [default-headers strict-transport]]
   [hyperlith.impl.html :as h]
   [hyperlith.impl.json :as json]
   [hyperlith.impl.router :as router]
   [hyperlith.impl.util :as u]
   [hyperlith.impl.zstd :as zstd]
   [manifold.deferred :as d]
   [manifold.stream :as s])
  (:import
   (java.util.concurrent ConcurrentHashMap)
   (hyperlith.impl.lane_context LaneCtx)
   (java.nio ByteBuffer)))

(def datastar-source-map
  (static-asset
    {:body         (u/load-resource "datastar.js.map")
     :content-type "text/javascript"
     :compress?    true}))

(def datastar
  (static-asset
    {:body
     (-> (u/load-resource "datastar.js") slurp
       ;; Make sure we point to the right source map
       (str/replace "datastar.js.map" datastar-source-map))
     :content-type "text/javascript"
     :compress?    true}))

(def on-load-js
  ;; Quirk with browsers is that cache settings are per URL not per
  ;; URL + METHOD this means that GET and POST cache headers can
  ;; mess with each other. To get around this an unused query param
  ;; is added to the url.

  ;; Retry Infinity means we always try to reconnect. The other defaults
  ;; mean that this will at most take 30s (default max backoff).
  "@post(window.location.pathname + (window.location.search + '&u=').replace(/^&/,'?'), {retryMaxCount: Infinity, openWhenHidden: false, retry: 'error'})")

(def tabid-js
  ;; Higher collision risk is acceptable here as it only needs to be
  ;; unique against a given users other tabs.
  "self.crypto.randomUUID().substring(0,8)")

(defn build-shim-page-resp [head-hiccup]
  (let [body (-> [h/doctype-html5
                  [:html  {:lang "en"}
                   [:head
                    [:meta {:charset "UTF-8"}]
                    (when head-hiccup head-hiccup)
                    ;; Scripts
                    [:script {:id    "js"
                              :defer true :type "module"
                              :src   datastar}]
                    ;; Enables responsiveness on mobile devices
                    [:meta {:name    "viewport"
                            :content "width=device-width, initial-scale=1.0"}]]
                   [:body
                    [:div {:data-signals:tabid tabid-js}]
                    [:div {:data-init              on-load-js
                           ;; Reconnect when the user comes online after
                           ;; being offline. Closes any existing connection
                           ;; from this div.
                           :data-on:online__window on-load-js}]
                    [:noscript "Your browser does not support JavaScript!"]
                    [:main {:id "morph"}]]]]
               h/html->bytes-oneshot)]
    (-> {:status  200
         :headers (assoc default-headers "Content-Encoding" "zstd")
         :body    (-> body (zstd/compress 19))}
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

(def event-prefix
  (String/.getBytes "event: datastar-patch-elements\ndata: elements "))

(def event-sufix
  (String/.getBytes "\n\n"))

(defn html->stream!
  [root ^LaneCtx lane-ctx]
  (let [buf ^ByteBuffer (.html-dst-buf lane-ctx)]
    (ByteBuffer/.clear buf)
    (run!
      (fn [node]
        (ByteBuffer/.put buf ^bytes event-prefix)
        (h/html->stream node lane-ctx buf)
        (ByteBuffer/.put buf ^bytes event-sufix))
      root)
    (.flip buf)))

(defn render-handler
  [path render-fn & {:keys [on-close on-open zstd-level zstd-window] :as _opts
                     :or   {zstd-level  -1 ;; twice as fast as 1
                            zstd-window 19}}]
  (router/add-route! [:post path]
    (fn handler [req]
      (let [zstd-ctx  (zstd/ctx zstd-level zstd-window)
            zstd-dst  (ByteBuffer/allocateDirect  (* 2 16384))
            lane-ctx  ((req :hyperlith.core/select-lane))
            stream    (s/stream 0 nil)
            last-put_ (atom nil)
            conns     (.lane-conns ^LaneCtx lane-ctx)
            ;; Only merge ctx at the start of a connection (so cheap)
            req       (-> (u/fast-merge req (.dbs ^LaneCtx  lane-ctx))
                        (assoc :hyperlith.core/lane-ctx lane-ctx))
            render
            (fn render []
              (try
                (if-not (s/closed? stream)
                  ;; Only render again if previous event was sent
                  ;; this gives you back pressure and frame dropping.
                  (when (or (nil? @last-put_) (d/realized? @last-put_))
                    (when-some [new-view (render-fn req)]
                      (let [html-dst ^ByteBuffer (.html-dst-buf
                                                   ^LaneCtx lane-ctx)
                            _        (html->stream! new-view lane-ctx)
                            _        (zstd/compress-chunk! zstd-ctx
                                       zstd-dst html-dst)]
                        (->> (s/put! stream zstd-dst)
                          (reset! last-put_)))))
                  (do
                    (ConcurrentHashMap/.remove conns
                      (System/identityHashCode render))
                    (zstd/close-ctx zstd-ctx)
                    (when on-close (on-close req))))
                (catch Throwable t
                  (repl-caught t))))]
        (ConcurrentHashMap/.put conns
          (System/identityHashCode render) render)
        (when on-open (on-open req))
        {:status  200
         :headers (assoc default-headers
                    "Content-Type"  "text/event-stream"
                    "Cache-Control" "no-store"
                    "Content-Encoding" "zstd")
         :body    stream}))))

(defn patch-signals [signals]
  [:div {:data-signals (json/edn->json signals)
         :data-init    "el.remove()"}])

(defn execute-expr [id expr]
  [:div {:id id :data-ignore-morph true :style {:display "none"}}
   [:div {:data-init (str expr ";el.remove()")}]])

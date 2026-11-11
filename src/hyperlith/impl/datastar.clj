(ns hyperlith.impl.datastar
  (:require
   [clojure.main :refer [repl-caught]]
   [clojure.string :as str]
   [hyperlith.impl.assets :refer [static-asset]]
   [hyperlith.impl.crypto :as crypto]
   [hyperlith.core :as-alias h]
   [hyperlith.impl.headers :refer [strict-transport]]
   [hyperlith.impl.html :as html]
   [hyperlith.impl.json :as json]
   [hyperlith.impl.util :as u]
   [hyperlith.impl.zstd :as zstd]
   [manifold.deferred :as d]
   [manifold.stream :as s])
  (:import
   (java.util.concurrent ConcurrentHashMap$KeySetView)
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
       (str/replace "datastar.js.map" (datastar-source-map :path)))
     :content-type "text/javascript"
     :compress?    true}))

(def on-load-js
  ;; Quirk with browsers is that cache settings are per URL not per
  ;; URL + METHOD this means that GET and POST cache headers can
  ;; mess with each other. To get around this an unused query param
  ;; is added to the url.

  ;; Retry Infinity means we always try to reconnect. We only retry on
  ;; network error.
  "@post(window.location.pathname + (window.location.search + '&u=').replace(/^&/,'?'), {retryInterval: 1000, retryScaler: 1, retryMaxCount: Infinity, openWhenHidden: false, retry: 'auto'})")

(def tabid-js
  ;; Higher collision risk is acceptable here as it only needs to be
  ;; unique against a given users other tabs.
  "self.crypto.randomUUID().substring(0,8)")

(defn build-shim-page-resp [head-hiccup]
  (let [body (-> [html/doctype-html5
                  [:html  {:lang "en"}
                   [:head
                    [:meta {:charset "UTF-8"}]
                    (when head-hiccup head-hiccup)
                    ;; Scripts
                    [:script {:id    "js"
                              :defer true :type "module"
                              :src   (datastar :path)}]
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
               html/html->bytes-oneshot)]
    {:status  200
     ;; Etags ensure the shim is only sent again if it's contents have changed
     :headers {"Content-Encoding" "zstd"
               "ETag"             (crypto/digest-base64 body)}
     :body    (-> body (zstd/compress 19))}))

(defn- shim-handler [{:keys! [shim-headers]}]
  (let [resp (build-shim-page-resp shim-headers)
        etag ((:headers resp) "ETag")]
    (fn handler [req]
      (if (= ((:headers req) "if-none-match") etag)
        {:status 304}
        resp))))

(defn action-handler [thunk {:keys! [path]}]
  {:path   path
   :routes {[:post path]
            (fn handler [req]
              (let [tx! ((req ::h/db-txs!) (req ::h/domain))]
                (-> (assoc req ::h/tx! tx!)
                  ;; Remove stuff that we don't want to pass on to the handler
                  (dissoc ::h/db-txs! ::h/conns ::h/domain ::h/stream-fn)
                  thunk))
              {:status 204})}})

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
        (html/html->stream node lane-ctx buf)
        (ByteBuffer/.put buf ^bytes event-sufix))
      root)
    (.flip buf)))

(defn- render-handler
  [render-fn {:keys [on-close on-open zstd-level zstd-window] :as _opts
              :or   {zstd-level  -1 ;; twice as fast as 1
                     zstd-window 19}}]
  (fn handler [req]
    (let [zstd-ctx       (zstd/ctx zstd-level zstd-window)
          zstd-dst       (ByteBuffer/allocateDirect  (* 2 16384))
          stream         ((req ::h/stream-fn))
          last-put_      (atom nil)
          last-slim-view (atom nil)
          conns          (req ::h/conns)
          domain         (req ::h/domain)
          ;; Remove stuff that we don't want to pass on to the handler
          tx!            ((req ::h/db-txs!) (req ::h/domain))
          req            (-> (assoc req ::h/tx! tx!)
                           (dissoc ::h/db-txs! ::h/conns ::h/domain
                             ::h/stream-fn))
          render
          (fn render [^LaneCtx lane-ctx]
            (try
              (if-not (s/closed? stream)
                ;; Only render again if previous event was sent
                ;; this gives you back pressure and frame dropping.
                (when (or (nil? @last-put_) (d/realized? @last-put_))
                  (let [new-view      (render-fn
                                        {:db ((.dbs lane-ctx) domain)}
                                        req)
                        new-slim-view (html/slim-view new-view)
                        change?       (not (= new-slim-view @last-slim-view))]
                    (when change?
                      (let [html-dst ^ByteBuffer (.html-dst-buf
                                                   ^LaneCtx lane-ctx)
                            _        (html->stream! new-view lane-ctx)
                            _        (zstd/compress-chunk! zstd-ctx
                                       zstd-dst html-dst)]
                        (->> (s/put! stream zstd-dst)
                          (reset! last-put_)))
                      (reset! last-slim-view new-slim-view))))
                (do
                  (ConcurrentHashMap$KeySetView/.remove conns render)
                  (zstd/close-ctx zstd-ctx)
                  (when on-close (on-close req))))
              (catch Throwable t
                (repl-caught t))))]
      (ConcurrentHashMap$KeySetView/.add conns render)
      (when on-open (on-open req))
      {:status  200
       :headers {"Content-Type"     "text/event-stream"
                 "Cache-Control"    "no-store"
                 "Content-Encoding" "zstd"}
       :body    stream})))

(defn view-handler [render-fn {:keys! [path] :as opts}]
  {:path   path
   :routes {[:get path]  (shim-handler opts)
            [:post path] (render-handler render-fn opts)}})

(defn patch-signals [signals]
  [:div {:data-signals (json/edn->json signals)
         :data-init    "el.remove()"}])

(defn execute-expr [id expr]
  [:div {:id id :data-ignore-morph true :style {:display "none"}}
   [:div {:data-init (str expr ";el.remove()")}]])

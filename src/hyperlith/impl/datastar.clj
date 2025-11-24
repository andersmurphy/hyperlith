(ns hyperlith.impl.datastar
  (:require [hyperlith.impl.assets :refer [static-asset]]
            [hyperlith.impl.headers
             :refer [default-headers strict-transport]]
            [hyperlith.impl.util :as util]
            [hyperlith.impl.brotli :as br]
            [hyperlith.impl.crypto :as crypto]
            [hyperlith.impl.html :as h]
            [hyperlith.impl.json :as json]
            [hyperlith.impl.router :as router]
            [hyperlith.impl.engine :as engine]
            [clojure.string :as str]))

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

(defn patch-elements [elements]
  (str "event: datastar-patch-elements"
    "\ndata: elements " (str/replace elements "\n" "\ndata: elements ")
    "\n\n\n"))

(defn patch-append-body [elements]
  (str "event: datastar-patch-elements"
    "\ndata: selector body"
    "\ndata: mode append"
    "\ndata: elements " (str/replace elements "\n" "\ndata: elements ")
    "\n\n\n"))

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
                      [:div {:data-signals:tabid tabid-js}]
                      [:div {:data-init              on-load-js
                             ;; Reconnect when the user comes online after
                             ;; being offline. Closes any existing connection
                             ;; from this div.
                             :data-on:online__window on-load-js}]
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
  [path render-fn & {:keys [_on-close _on-open] :as opts}]
  (router/add-route! [:post path]
    (engine/render-handler opts
      (fn [req] (-> (render-fn req) h/html->str patch-elements)))))

(defn patch-signals [signals]
  (h/html [:div {:data-signals (json/edn->json signals)
                 :data-init    "el.remove()"}]))

(defn execute-expr [expr]
  (h/html [:div {:data-init (str expr ";el.remove()")}]))


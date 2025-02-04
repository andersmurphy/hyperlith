(ns hyperlith.impl.datastar
  (:require [hyperlith.impl.session :refer [csrf-cookie-js]]
            [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.assets :refer [static-asset]]
            [hyperlith.impl.gzip :as gz]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [hiccup2.core :as h])
  (:import (java.io InputStream)))

(def ^:private datastar-source-map
  (static-asset
    {:body         (-> "datastar.js.map" io/resource slurp)
     :content-type "text/javascript"
     :gzip?        true}))

(def ^:private datastar
  (static-asset
    {:body
     (-> "datastar.js" io/resource slurp
       ;; Make sure we point to the right source map
       (str/replace "datastar.js.map" (:path datastar-source-map)))
     :content-type "text/javascript"
     :gzip?        true}))

(def ^:private icon
  (static-asset
    {:body         (-> "icon.png" io/resource io/input-stream
                     InputStream/.readAllBytes)
     :content-type "image/png"}))

(def ^:private doctype-html5 "<!DOCTYPE html>")

(defn build-shim-page-resp [{:keys [path css-path]}]
  {:status  200
   :headers (assoc default-headers "Content-Encoding" "gzip")
   :body
   (->> (h/html
          [:html  {:lang "en"}
           [:head
            [:meta {:charset "UTF-8"}]
            [:link {:rel "icon" :type "image/png" :href (icon :path)}]
            ;; Styles
            (when css-path
              [:link#css {:rel  "stylesheet" :type "text/css"
                          :href css-path}])
            ;; Scripts
            [:script#js {:defer true :type "module"
                         :src   (datastar :path)}]
            ;; Enables responsiveness on mobile devices
            [:meta {:name    "viewport"
                    :content "width=device-width, initial-scale=1.0"}]]
           [:body
            [:div {:data-signals-csrf csrf-cookie-js}]
            [:div {:data-on-load
                   (str "@post('" (if (= path "/") "" path) "/updates')")}]
            [:noscript "Your browser does not support JavaScript!"]
            [:main {:id "morph"}]]])
     (str doctype-html5)
     gz/gzip)})

(defn merge-fragments [fragments]
  (str "event: datastar-merge-fragments\ndata: fragments "
    (str/replace fragments "\n" "\ndata: fragments ")
    "\n\n\n"))

(def default-routes
  {[:get (datastar :path)]            (datastar :handler)
   [:get (datastar-source-map :path)] (datastar-source-map :handler)
   [:get (icon :path)]                (icon :handler)})

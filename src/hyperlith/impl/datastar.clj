(ns hyperlith.impl.datastar
  (:require [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.session :refer [get-csrf-cookie-expr]]
            [hyperlith.impl.gzip :refer [gzip]]            
            [clojure.string :as str]
            [clojure.java.io :as io]
            [hiccup2.core :as h])
  (:import (java.io InputStream)))

(def ^:private datastar-source-map
  {:status  200
   :headers (assoc default-headers
              "Content-Type"     "text/javascript"
              "Content-Encoding" "gzip")
   :body    (-> "datastar.js.map" io/resource slurp gzip)})

(def ^:private datastar
  {:path    "/datastar-v1.0.0-beta.1.js"
   :status  200
   :headers (assoc default-headers
              "Content-Type"     "text/javascript"
              "Content-Encoding" "gzip")
   :body    (-> "datastar.js" io/resource slurp gzip)})

(def ^:private icon
  {:path    "/icon.png"
   :status  200
   :headers (assoc default-headers "Content-Type" "image/png")
   :body    (-> "icon.png" io/resource io/input-stream
              InputStream/.readAllBytes)})

(def ^:private doctype-html5 "<!DOCTYPE html>")

(defn build-shim-page-resp [{:keys [path]}]
  {:status  200
   :headers (assoc default-headers "Content-Encoding" "gzip")
   :body
   (->> (h/html
          [:html  {:lang "en"}
           [:head
            [:meta {:charset "UTF-8"}]
            [:link {:rel  "icon" :type "image/png"
                    :href (icon :path)}]
            ;; Scripts
            [:script {:defer true :type "module"
                      :src   (datastar :path)}]
            ;; Enables responsiveness on mobile devices
            [:meta {:name    "viewport"
                    :content "width=device-width, initial-scale=1.0"}]]
           [:body
            [:div {:data-signals-csrf get-csrf-cookie-expr}]
            [:div {:data-on-load
                   (str "@post('" (if (= path "/") "" path) "/updates')")}]
            [:noscript "Your browser does not support JavaScript!"]
            [:main {:id "morph"}]]])
     (str doctype-html5)
     gzip)})

(defn merge-fragments [fragments]
  (str "event: datastar-merge-fragments\ndata: fragments "
              (str/replace fragments "\n" "\ndata: fragments ")
              "\n\n\n"))

(def default-routes
  {[:get (datastar :path)]   (fn [_] datastar)
   [:get "/datastar.js.map"] (fn [_] datastar-source-map)
   [:get (icon :path)]       (fn [_] icon)})

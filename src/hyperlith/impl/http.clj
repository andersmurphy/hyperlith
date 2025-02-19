(ns hyperlith.impl.http
  (:require [org.httpkit.client :as http]
            [hyperlith.impl.json :as json]))

(defn get! [url request]
  #_:clj-kondo/ignore
  (cond-> @(http/get url request)
    :body (update :body json/try-json->edn)))

(defn post! [url request]
  #_:clj-kondo/ignore
  (cond-> @(http/post url request)
    :body (update :body json/try-json->edn)))


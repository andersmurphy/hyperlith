(ns hyperlith.impl.http
  (:require [org.httpkit.client :as http]
            [hyperlith.impl.json :as json]))

(defn- try-json->edn [json]
  (try (json/json->edn json)
       (catch Throwable _)))

(defn get! [url request]
  #_:clj-kondo/ignore
  (cond-> @(http/get url request)
    :body (update :body try-json->edn)))

(defn post! [url request]
  #_:clj-kondo/ignore
  (cond-> @(http/post url request)
    :body (update :body try-json->edn)))


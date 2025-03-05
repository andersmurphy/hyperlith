(ns hyperlith.impl.http
  (:require [org.httpkit.client :as http]
            [hyperlith.impl.json :as json]))
(defn has-json-body? [resp]
  (and (:body resp)
    (-> resp :headers :content-type (= "application/json"))))

(defn get! [url request]
  #_:clj-kondo/ignore
  (let [req @(http/get url request)]
    (cond-> req
      (has-json-body? req) (update :body json/try-json->edn))))

(defn post! [url request]
  #_:clj-kondo/ignore
  (let [req @(http/post url request)]
    (cond-> req
      (has-json-body? req) (update :body json/try-json->edn))))


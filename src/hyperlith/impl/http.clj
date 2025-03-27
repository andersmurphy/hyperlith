(ns hyperlith.impl.http
  (:require [org.httpkit.client :as http]
            [hyperlith.impl.json :as json]))

(defn has-json-body? [resp]
  (and (:body resp)
    (-> resp :headers :content-type (= "application/json"))))

(defn wrap-json-response [method]
  (fn [url request]
    (let [req @(method url request)]
      (cond-> req
        (has-json-body? req) (update :body json/json->edn)))))

(def get! (wrap-json-response #_:clj-kondo/ignore http/get))

(def post! (wrap-json-response #_:clj-kondo/ignore http/post))



(ns hyperlith.impl.json
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn- json-stream->edn [json]
  (-> json io/reader (json/read {:key-fn keyword})))

(defn json->edn [json]
  (json/read-str json {:key-fn keyword}))

(defn edn->json [edn]
  (json/write-str edn))

(defn- parse-json-body? [req]
  (and (= (:request-method req) :post)
    (= (:content-type req) "application/json")
    (:body req)))

(defn wrap-parse-json-body
  [handler]
  (fn [req]
    (cond-> req
      (parse-json-body? req) (update :body json-stream->edn)
      true                   handler)))

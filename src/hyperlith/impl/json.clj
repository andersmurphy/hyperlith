(ns hyperlith.impl.json
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn json-stream->edn [json]
  (try
    (-> json io/reader (json/read {:key-fn keyword}))
    (catch Throwable _)))

(defn json->edn [json]
  (try (json/read-str json {:key-fn keyword})
       (catch Throwable _)))

(defn edn->json [edn & {:as opts}]
  (json/write-str edn opts))

(defn edn->pretty-json [edn]
  (with-out-str
    (json/pprint edn :escape-unicode false)))

(defn parse-json-body? [req]
  (and (= (:request-method req) :post)
    (= (:content-type req) "application/json")
    (:body req)))

(defn wrap-parse-json-body
  [handler]
  (fn [req]
    (cond-> req
      (parse-json-body? req) (update :body json-stream->edn)
      true                   handler)))

(ns hyperlith.impl.http
  (:require [org.httpkit.client :as http]
            [hyperlith.impl.json :as json]
            [clojure.string :as str]))

(defn has-json-body? [resp]
  (and (:body resp)
    (-> resp :headers :content-type (str/starts-with? "application/json"))))

(defn wrap-json-response [method]
  (fn [url request]
    (let [req @(method url request)]
      (cond-> req
        (has-json-body? req) (update :body json/json->edn)))))

(def get! (wrap-json-response #_:clj-kondo/ignore http/get))

(def post! (wrap-json-response #_:clj-kondo/ignore http/post))

(defn throw-if-status-not!
  "Convert response status that is not in the status-set into an ex-info and
  then throw."
  [status-set message {:keys [body status]}]
  (if ((complement status-set) status) body
      (throw (ex-info (str message ": " status)
               (assoc body :status status)))))



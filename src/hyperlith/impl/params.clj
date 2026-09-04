(ns hyperlith.impl.params
  (:require [hyperlith.impl.codec :as codec]))

(defn parse-query-string [query-string]
  (codec/form-decode query-string))

(defn wrap-query-params
  [handler]
  (fn [req]
    (-> (if-let [query-string (:query-string req)]
          (assoc req :query-params (parse-query-string query-string))
          req)
      handler)))



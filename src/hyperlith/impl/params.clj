(ns hyperlith.impl.params
  (:require [ring.util.codec :as codec]))

;; TODO: might want to parse datastar query param (json) to edn
;; TODO: might want to keyword query params

(defn wrap-query-params
  [handler]
  (fn [req]
    (-> (if-let [query-string (:query-string req)]
          (assoc req :query-params (codec/form-decode query-string "UTF-8"))
          req)
      handler)))



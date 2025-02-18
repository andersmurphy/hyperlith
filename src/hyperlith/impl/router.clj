(ns hyperlith.impl.router
  (:require [hyperlith.impl.datastar :as ds]))

(defn router
  ([routes] (router routes (fn [_] {:status 404})))
  ([routes not-found-handler]
   (let [routes (merge ds/routes routes)]
     (fn [req]
       ((routes [(:request-method req) (:uri req)] not-found-handler) req)))))

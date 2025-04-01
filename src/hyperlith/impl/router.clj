(ns hyperlith.impl.router
  (:require [hyperlith.impl.datastar :as ds]))

(defn router
  "Convert route map into router."
  ([routes] (router routes (fn [_] {:status 404})))
  ([routes not-found-handler]
   (let [routes (merge ds/routes routes)]
     (fn [req]
       ((routes [(:request-method req) (:uri req)] not-found-handler) req)))))

(defn wrap-routes
  "Wrap a route map in a collection of middleware. Middlewares are applied
  left to right (top to bottom)."
  [middlewares routes]
  (update-vals routes (apply comp middlewares)))

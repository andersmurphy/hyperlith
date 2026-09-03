(ns hyperlith.impl.router)

(defonce routes_ (atom {}))

(defn add-route! [[method path :as _route] handler]
  (swap! routes_ assoc-in [method path] handler)
  path)

(defn- fallback [_] {:status 404})

(defn router [req]
  ((get (get @routes_ (:request-method req)) (:uri req) fallback) req))

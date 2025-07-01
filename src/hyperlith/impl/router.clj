(ns hyperlith.impl.router)

(defonce routes_ (atom {}))

(defn add-route! [[_method path :as route] handler]
  (swap! routes_ assoc route handler)
  path)

(defn router [req]
  ((@routes_ [(:request-method req) (:uri req)] (fn [_] {:status 404})) req))

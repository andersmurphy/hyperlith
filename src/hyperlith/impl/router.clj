(ns hyperlith.impl.router)

(defonce routes_ (atom {}))

(defn add-route! [[method path :as _route] handler]
  (swap! routes_ assoc-in [method path] handler)
  path)

(defn router [req]
  (let [fallback (fn [_] {:status 404})]
    ((get (get @routes_ (:request-method req)) (:uri req) fallback) req)))

(comment
  (def req {:request-method :get :uri "foo"})
  
  (add-route! [:get "foo"] (fn [_] "hello"))
  (user/bench (router req)))

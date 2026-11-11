(ns hyperlith.impl.router
  (:require [hyperlith.core :as-alias h]
            [clojure.string :as str]))

(defn by-domain [apps k]
  (into {} (map (fn [{:keys! [domain] :as app}]
                  [domain (app k)]))
    apps))

(defn router [apps mode]
  (let [fallback         (fn fallback [_] {:status 404})
        get-domain       (if (= mode :prod)
                           ;; for prod
                           (fn [req]
                             (let [header (:headers req)]
                               (or (get header ":authority")
                                 (get header "host"))))
                           ;; for localhost
                           (fn [req]
                             (-> (:headers req)
                               (get "host")
                               (str/split #"\.localhost.*")
                               first)))
        routes-by-domain (by-domain apps :routes)
        tx-by-domain     (by-domain apps :tx!)
        conns-by-domain  (by-domain apps :conns)]
    (fn [req]
      (let [domain (get-domain req)
            routes (routes-by-domain domain)]
        (if routes
          ((routes
             [(:request-method req) (:uri req)]
             fallback)
           (assoc req
             ::h/domain domain
             ::h/tx! (tx-by-domain domain)
             ::h/conns (conns-by-domain domain)))
          (fallback req))))))

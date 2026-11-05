(ns hyperlith.impl.router
  (:require [hyperlith.core :as-alias h]
            [hyperlith.impl.trace :as t]
            [clojure.string :as str]))

(defn router [apps mode]
  (let [fallback   (fn fallback [_] {:status 404})
        get-domain (if (= mode :prod)
                     ;; for prod
                     (fn [req]
                       (get-in req [:headers ":authority"]))
                     ;; for localhost
                     (fn [req] (-> (get-in req [:headers "host"])
                                 (str/split #"\.localhost.*")
                                 first)))
        routers    (into {} (map (fn [{:keys! [domain routes]}]
                                   [domain routes]))
                     apps)]
    (fn [req]
      (let [domain (get-domain req)
            router (routers domain)]
        (if router
          ((router
             [(:request-method req) (:uri req)]
             fallback)
           (assoc req ::h/domain domain))
          (fallback req))))))

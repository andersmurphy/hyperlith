(ns hyperlith.impl.assets
  (:require [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.crypto :as crypto]
            [hyperlith.impl.gzip :as gz]))

(defn static-asset
  [{:keys [body content-type gzip?]}]
  (let [resp (cond-> {:status  200
                      :headers
                      (assoc default-headers
                        "Cache-Control" "max-age=31536000, immutable"
                        "Content-Type"  content-type)
                      :body    body}
               gzip? (update :body gz/gzip)
               gzip? (assoc-in [:headers "Content-Encoding"] "gzip"))]
    {:handler (fn [_] resp)
     :path    (str "/" (crypto/digest body))}))



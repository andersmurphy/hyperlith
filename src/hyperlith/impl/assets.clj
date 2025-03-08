(ns hyperlith.impl.assets
  (:require [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.crypto :as crypto]
            [hyperlith.impl.brotli :as br]))

(defn static-asset
  [{:keys [body content-type compress?]}]
  (let [resp (cond-> {:status  200
                      :headers
                      (assoc default-headers
                        "Cache-Control" "max-age=31536000, immutable"
                        "Content-Type"  content-type)
                      :body    body}
               compress? (update :body br/compress :quality 11)
               compress? (assoc-in [:headers "Content-Encoding"] "br"))]
    {:handler (fn [_] resp)
     :path    (str "/" (crypto/digest body))}))



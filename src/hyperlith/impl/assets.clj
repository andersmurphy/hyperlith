(ns hyperlith.impl.assets
  (:require [hyperlith.impl.headers :refer [default-headers]]
            [hyperlith.impl.crypto :as crypto]
            [hyperlith.impl.router :as router]
            [hyperlith.impl.zstd :as zstd]))

(defn static-asset
  [{:keys [body content-type compress?]}]
  (let [resp (cond-> {:status  200
                      :headers
                      (assoc default-headers
                        "Cache-Control" "max-age=31536000, immutable"
                        "Content-Type"  content-type)
                      :body    body}
               compress? (update :body zstd/compress 19)
               compress? (assoc-in [:headers "Content-Encoding"] "zstd"))
        path (str "/" (crypto/digest body))]
    (router/add-route! [:get path] (fn [_] resp))))



(ns hyperlith.impl.cache
  (:import [com.github.benmanes.caffeine.cache Caffeine Cache]))

(defn init ^Cache
  ([max-size]
   (-> (Caffeine/newBuilder)
     (.maximumSize max-size)
     (.build))))

(defn lookup-or-miss [^Cache cache k f]
  (.get cache k f))

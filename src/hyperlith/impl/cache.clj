(ns hyperlith.impl.cache
  (:refer-clojure :exclude [get])
  (:import [com.github.benmanes.caffeine.cache Caffeine Cache]))

(defn init ^Cache
  [{:keys [max-weight max-entries weigher executor]}]
  (cond-> (Caffeine/newBuilder)
    max-entries (.maximumSize   max-entries)
    max-weight  (.maximumWeight max-weight)
    weigher     (.weigher       weigher)
    executor    (.executor executor)
    true        (.build)))

(defn lookup-or-miss [^Cache cache k f]
  (.get cache k f))

(defn get [^Cache cache k]
  (.getIfPresent cache k))

(defn put [^Cache cache k v]
  (.put cache k v)
  v)

(defn clean-up! [^Cache cache]
  (.cleanUp cache))

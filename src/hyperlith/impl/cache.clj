(ns hyperlith.impl.cache
  (:import [com.github.benmanes.caffeine.cache Caffeine Cache]
           [java.util Arrays]))

(defn init ^Cache
  [{:keys [max-weight max-entries weigher]}]
  (cond-> (Caffeine/newBuilder)
    max-entries (.maximumSize   max-entries)
    max-weight  (.maximumWeight max-weight)
    weigher     (.weigher       weigher)
    true        (.build)))

(defn lookup-or-miss [^Cache cache k f]
  (.get cache k f))

(deftype BlobKey [^bytes b ^int hash]
  Object
  (hashCode [_] hash)
  (equals [_ o]
    (and (instance? BlobKey o)
      (== hash (.hash ^BlobKey o))
      (Arrays/equals ^bytes b ^bytes (.b ^BlobKey o)))))

(defn blob->key [^bytes x]
  (->BlobKey x (int (Arrays/hashCode x))))

(defn get [^Cache cache k]
  (.getIfPresent cache k))

(defn put [^Cache cache k v]
  (.put cache k v)
  v)

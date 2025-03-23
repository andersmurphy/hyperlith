(ns hyperlith.impl.tuples)

(defn tuples
  "Returns the set of tuples that represent a nested map/vector. Lets you
  use datalog query engines to query json/edn data."
  ([root] {:pre [(or (map? root) (vector? root))]}
   (tuples [] root))
  ([parent x]
   (cond (map? x)
         (mapcat (fn [[k v]] (tuples (conj parent k) v)) x)

         (vector? x)
         (mapcat (fn [i v] (tuples (conj parent i) v)) (range) x)

         :else [(conj parent x)])))


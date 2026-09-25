(ns hyperlith.impl.css
  (:require [hyperlith.impl.assets :refer [static-asset]]))

(let [to-str
      (fn to-str [s]
        (cond (keyword? s) (name s)
              (vector? s)  (->> (map to-str s)
                             (interpose " ")
                             (apply str))
              :else        (str s)))]

  (defn style-map->style [v]
    (reduce-kv (fn [acc k v]
                 (str acc (to-str k) ":" (to-str v)";"))
      ""
      (sort-by key v)))

  (defn format-rule [[k v]]
    (str
      (to-str k)
      (if (map? v)
        (str "{"
          (reduce-kv (fn [acc k v]
                       (str acc (to-str k) ":" (to-str v)";"))
            ""
            (sort-by key v))
          "}")
        v)))

  (defn flatten-seq [xs]
    (mapcat (fn [x] (if (and (seq? x) (vector? (first x))) x [x])) xs))

  (defn static-css [css-rules]
    (static-asset
      {:body         (if (vector? css-rules)
                       (->> (flatten-seq css-rules)
                         (map format-rule)
                         (reduce str ""))
                       css-rules)
       :content-type "text/css"
       :compress?    true}))

  (defn -- [css-var-name]
    (str "var(--" (to-str css-var-name) ")")))







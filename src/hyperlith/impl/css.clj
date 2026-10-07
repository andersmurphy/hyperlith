(ns hyperlith.impl.css
  (:require [hyperlith.impl.assets :refer [static-asset]]
            [clojure.string :as str]))

(let [to-str
      (fn to-str
        ([s]
         (cond (keyword? s) (name s)
               (vector? s)  (str/join " " (mapv to-str s))
               :else        (str s)))
        ([^StringBuilder sb s]
         (cond (keyword? s) (.append sb ^String (name s))
               (vector? s)
               (loop [more (next s)
                      sep  " "]
                 (when more
                   (.append sb ^String sep)
                   (to-str sb (first more))
                   (recur (next more) sep)))
               :else        (.append sb s))
         sb))]


  (defn style-map->style [^StringBuilder sb v]
    (reduce-kv (fn [^StringBuilder sb k v]
                 (doto sb
                   (to-str k)
                   (.append":")
                   (to-str v)
                   (.append ";")))
      sb
      v)
    (let [s (StringBuilder/.toString sb)]
      (.setLength sb 0)
      s))

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

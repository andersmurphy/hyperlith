(ns hyperlith.impl.css
  (:require [hyperlith.impl.assets :refer [static-asset]]))

(defn to-str [s]
  (cond (keyword? s) (name s)
        (vector? s)  (->> (map to-str s)
                       (interpose " ")
                       (apply str))
        :else        (str s)))

(defn format-rule [[k v]]
  (str
    (to-str k)
    (if (map? v)
      (str "{"
        (reduce (fn [acc [k v]]
                  (str acc (to-str k) ":" (to-str v)";"))
          ""
          (sort-by (comp to-str key) v))
        "}")
      v)))

(defn static-css [css-rules]
  (static-asset
    {:body         (if (vector? css-rules)
                     (->> (map format-rule css-rules) (reduce str ""))
                     css-rules)
     :content-type "text/css"
     :compress?    true}))

(defn -- [css-var-name]
  (str "var(--" (to-str css-var-name) ")"))

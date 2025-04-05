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
    "{"
    (reduce (fn [acc [k v]]
              (str acc (to-str k) ":" (to-str v) ";"))
      ""
      (sort-by (comp to-str key) v))
    "}"))

(defn static-css [& css-rule-sources]
  (static-asset
    {:body         (reduce
                     (fn [result css-rule-source]
                       (apply str result
                         (if (vector? css-rule-source)
                           (mapv format-rule css-rule-source)
                           [css-rule-source])))
                     ""
                     css-rule-sources)
     :content-type "text/css"
     :compress?    true}))

(defn -- [css-var-name]
  (str "var(--" (to-str css-var-name) ")"))

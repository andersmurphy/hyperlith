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

(defn static-css [css-rules]
  (static-asset
    {:body         (if (string? css-rules) css-rules
                       (->> (map format-rule css-rules) (reduce str "")))
     :content-type "text/css"
     :gzip?        true}))

(defn -- [css-var-name]
  (str "var(--" (to-str css-var-name) ")"))

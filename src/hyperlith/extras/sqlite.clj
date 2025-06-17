(ns hyperlith.extras.sqlite
  (:require [hyperlith.impl.namespaces :refer [import-vars]]
            [sqlite4clj.core :as d]
            [honey.sql :as hsql]))

(import-vars
  [sqlite4clj.core
   init-db!
   with-read-tx
   with-write-tx
   encode
   decode])

;;; - QUERY FUNCTIONS -

(defn format-query [query]
  (cond
    (vector? query) query
    (string? query) [query]
    :else           (hsql/format query {:checking :strict})))

(defn q [db query]
  (let [result (d/q db (format-query query))]
    (when (seq result) result)))

;;; - UTILITY -

(defn table-info [db table-name]
  (q db (str "PRAGMA table_info(" (-> table-name name) ");")))

(defn table-list [db]
  (q db "PRAGMA table_list;"))

(defn pragma-check [db]
  (->> [(q db "pragma foreign_keys")
        (q db "pragma journal_mode")
        (q db "pragma synchronous")
        (q db "pragma page_size")
        (q db "pragma cache_size")
        (q db "pragma temp_store")]))

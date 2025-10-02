(ns hyperlith.extras.sqlite
  (:require [hyperlith.impl.namespaces :refer [import-vars]]
            [sqlite4clj.core :as d]
            [sqlite4clj.litestream]
            [honey.sql :as hsql]))

(import-vars
  [sqlite4clj.core
   init-db!
   with-read-tx
   with-write-tx]
  [sqlite4clj.litestream
   restore-then-replicate!
   restore-to-path!])

(defmacro q [db [query-type query :as string-query] & [params]]
  (if (string? query-type)
    `(d/q ~db ~string-query)
    `(d/q ~db ~(hsql/format query {:params params ;; :checking :strict
                                   }))))

(def format-query hsql/format)

;;; - UTILITY -

(defn table-info [db table-name]
  (let [t-name (-> table-name name)]
    (q db ["PRAGMA table_info(?);" t-name])))

(defn table-list [db]
  (q db ["PRAGMA table_list;"]))

(defn pragma-check [db]
  (->> [(q db ["pragma foreign_keys"])
        (q db ["pragma journal_mode"])
        (q db ["pragma synchronous"])
        (q db ["pragma page_size"])
        (q db ["pragma cache_size"])
        (q db ["pragma temp_store"])]))

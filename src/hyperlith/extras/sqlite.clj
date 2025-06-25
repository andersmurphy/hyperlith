(ns hyperlith.extras.sqlite
  (:require [hyperlith.impl.namespaces :refer [import-vars]]
            [sqlite4clj.core :as d]
            [honey.sql :as hsql]))

(import-vars
  [sqlite4clj.core
   init-db!
   with-read-tx
   with-write-tx])

;;; - QUERY FUNCTIONS -
(defn q [db query]
  (->> (if (vector? query) query (hsql/format query))
    (d/q db)))

;; (defmacro q [db query & [opts]]
;;   `(d/q ~db ~(hsql/format query {:params opts})))

;;; - UTILITY -

(defn table-info [db table-name]
  (q db [(str "PRAGMA table_info(" (-> table-name name) ");")]))

(defn table-list [db]
  (q db ["PRAGMA table_list;"]))

(defn pragma-check [db]
  (->> [(q db ["pragma foreign_keys"])
        (q db ["pragma journal_mode"])
        (q db ["pragma synchronous"])
        (q db ["pragma page_size"])
        (q db ["pragma cache_size"])
        (q db ["pragma temp_store"])]))

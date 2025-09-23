(ns hyperlith.extras.sqlite
  (:require [hyperlith.impl.namespaces :refer [import-vars]]
            [sqlite4clj.core :as d]
            [honey.sql :as hsql]
            [clojure.java.process :as proc]))

(import-vars
  [sqlite4clj.core
   init-db!
   with-read-tx
   with-write-tx])

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

(defn restore-from-litestream!
  "This is a noop if db already exists. Throws error if litestream is not
  present. You want to know if your backups are not working."
  [db-name]
  (proc/exec "litestream" "restore" db-name))

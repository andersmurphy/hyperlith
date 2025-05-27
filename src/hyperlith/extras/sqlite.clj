(ns hyperlith.extras.sqlite
  (:require [hyperlith.impl.namespaces :refer [import-vars]]
            [sqlite4clj.core :as d]
            [honey.sql :as hsql]))

(import-vars
  [sqlite4clj.core
   with-read-tx
   with-write-tx])

(defn init-db!
  [url & [{:keys [pool-size pragma] :or {pool-size 4}}]]
  (let [;; Only one write connection
        write
        (d/init-db! url
          {:pool-size 1}
          :pragma pragma)
        ;; Pool of read connections
        read
        (d/init-db! url
          {:read-only true
           :pool-size pool-size
           :pragma    pragma})]
    {:db-read  read
     :db-write write}))

;;; - QUERY FUNCTIONS -

(defn format-query [query]
  (cond
    (vector? query) query
    (string? query) [query]
    :else           (hsql/format query
                      {:checking :strict})))

(defn q [db query]
  (let [result (d/q db (format-query query))]
    (when (seq result) result)))

;;; - UTILITY -

(defn table-info [db table-name]
  (q db (str "PRAGMA table_info(" (-> table-name name) ");")))

(defn table-list [db]
  (q db "PRAGMA table_list;"))

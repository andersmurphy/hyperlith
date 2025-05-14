(ns hyperlith.extras.sqlite
  (:require [hyperlith.impl.namespaces :refer [import-vars]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql]
            [next.jdbc.result-set :as rs]
            [next.jdbc.connection :as connection]
            [honey.sql :as hsql])
  (:import (com.zaxxer.hikari HikariDataSource)))

(import-vars
  [next.jdbc
   with-transaction]
  [next.jdbc.sql
   insert-multi!])

;;; - DB INITIALISATION -

(def data-source-properties
  ;; For more info on sqlite-jdbc properties
  ;; https://github.com/xerial/sqlite-jdbc/blob/master/src/main/java/org/sqlite/SQLiteConfig.java#L376
  {;; https://phiresky.github.io/blog/2020/sqlite-performance-tuning/
   ;; https://www.sqlite.org/pragma.html#pragma_cache_size
   ;; (/ 6.4e+7 4096) -> 15625
   ;; (* 15625 4096 1e-6) -> 64MB
   :cache_size         15625
   :page_size          4096
   ;; https://www.sqlite.org/wal.html
   ;; https://www.sqlite.org/pragma.html#pragma_journal_mode
   :journal_mode       "WAL"
   ;; https://www.sqlite.org/pragma.html#pragma_synchronous
   :synchronous        "NORMAL"
   :temp_store         "memory"})

(def result-set-options
  {:builder-fn rs/as-unqualified-lower-maps})

(defn init-db!
  [url & [{:keys [pool-size pragma] :or {pool-size 4}}]]
  (let [;; Pool of read connections
        read
        (jdbc/with-options
          (connection/->pool
            HikariDataSource
            {;; https://github.com/brettwooldridge/HikariCP#readme
             :jdbcUrl              url
             :minimumIdle          pool-size
             ;; connections = core_count * 2
             :maximumPoolSize      pool-size
             :readOnly             true
             :dataSourceProperties
             (merge data-source-properties
               ;; Needed for read only
               {:open_mode 1}
               pragma)})
          result-set-options)
        ;; Only one write connection
        write
        (jdbc/with-options
          (connection/->pool
            HikariDataSource
            {:jdbcUrl         url
             :minimumIdle     1
             :maximumPoolSize 1
             :dataSourceProperties
             (merge data-source-properties
               {:jdbcUrl          url
                ;; This is more performant than DEFERRED
                :transaction_mode "IMMEDIATE"}
               pragma)})
          result-set-options)]
    {:db-read  read
     :db-write write}))

;;; - QUERY FUNCTIONS -

(defn format-query [query]
  (cond
    (vector? query) query
    (string? query) [query]
    :else           (hsql/format query
                      {:checking :strict})))

(defn q [db query & xfs]
  (let [result (jdbc/execute! db (format-query query)
                 (assoc result-set-options                   
                  :return-keys (not (:no-return query))))]
    (if xfs
      (into [] (apply comp xfs) result)
      result)))

;;; - UTILITY -

(defn table-info [db table-name]
  (q db (str "PRAGMA table_info(" (-> table-name name) ");")))

(defn table-list [db]
  (q db "PRAGMA table_list;"))

(defn pragma-check [db]
  (->> [(q db "pragma main.foreign_keys")
        (q db "pragma main.journal_mode")
        (q db "pragma main.synchronous")
        (q db "pragma main.page_size")
        (q db "pragma main.cache_size")
        (q db "pragma main.temp_store")]
    (map first)
    (apply merge)))



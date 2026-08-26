(ns hyperlith.impl.sqlite
  (:require [sqlite4clj.impl.api :as api]
            [clojure.core.cache.wrapped :as cache]
            [honey.sql :as hsql]))

(defn- bind [stmt params]
  (reduce
    (fn [i param]
      (cond
        (integer? param) (api/bind-int    stmt i param)
        (double? param)  (api/bind-double stmt i param)
        (string? param)  (api/bind-text   stmt i param)
        (nil? param)     (api/bind-null   stmt i)
        :else            (api/bind-blob   stmt i param))
      (inc i))
    1 ;; starts at 1
    params))

(defn- prepare
  ([pdb sql]
   (let [stmt      (api/prepare-v3 pdb sql)
         col-count (int #_{:clj-kondo/ignore [:type-mismatch]}
                     (api/column-count stmt))]
     (cond-> {:stmt stmt}
       (> col-count 0)
       (assoc :col-metadata
         (mapv (fn [c]
                 {:database (api/column-database-name stmt c)
                  :table    (api/column-table-name stmt c)
                  :origin   (api/column-origin-name stmt c)
                  :alias    (api/column-name stmt c)})
           (range 0 col-count)))))))

(defn- prepare-cached [{:keys [pdb stmt-cache]} query]
  (let [sql    (first query)
        params (subvec query 1)
        {:keys [stmt] :as m}
        (cache/lookup-or-miss stmt-cache sql
          (fn [_] (prepare pdb sql)))]
    (bind stmt params)
    m))

(defmacro with-stmt-reset
  {:clj-kondo/lint-as 'clojure.core/with-open}
  [[stmt-binding stmt] & body]
  `(let [~stmt-binding ~stmt]
     (try
       ~@body
       (finally
         (api/reset ~stmt-binding)
         (api/clear-bindings ~stmt-binding)))))

(defn- get-column-val [stmt n]
  (case (int #_{:clj-kondo/ignore [:type-mismatch]}
          (api/column-type stmt n))
    ;; See type codes here: https://sqlite.org/c3ref/c_blob.html
    1 (api/column-int    stmt n)
    2 (api/column-double stmt n)
    3 (api/column-text   stmt n)
    4 (api/column-blob   stmt n)
    5 nil))

(defn- column [stmt n-cols]
  (case n-cols
    0 nil
    1 [(get-column-val stmt 0)]
    2 [(get-column-val stmt 0)
       (get-column-val stmt 1)]
    3 [(get-column-val stmt 0)
       (get-column-val stmt 1)
       (get-column-val stmt 2)]
    4 [(get-column-val stmt 0)
       (get-column-val stmt 1)
       (get-column-val stmt 2)
       (get-column-val stmt 3)]
    5 [(get-column-val stmt 0)
       (get-column-val stmt 1)
       (get-column-val stmt 2)
       (get-column-val stmt 3)
       (get-column-val stmt 4)]
    ;; After 5 params it's worth iterating
    (loop [n    0
           cols (transient [])]
      (if (>= n n-cols)
        (persistent! cols)
        (recur (inc n)
          (conj! cols (get-column-val stmt n)))))))

(defn- unwrap-result-set-fn
  [col-metadata result-set]
  (let [result (if (= (count col-metadata) 1)
                 (into [] cat result-set)
                 (into [] result-set))]
    (when (seq result) result)))

(defn q* [conn query]
  (let [{:keys [stmt col-metadata]} (prepare-cached conn query)]
    (with-stmt-reset [stmt stmt]
      (let [n-cols        (int
                            #_{:clj-kondo/ignore [:type-mismatch]}
                            (api/column-count stmt))
            ;; Could be passed in but keeping it simple for now.
            result-set-fn unwrap-result-set-fn]
        (result-set-fn col-metadata
          (reify
            clojure.lang.IReduceInit
            (reduce [_ f init]
              (loop [ret init]
                (let [code (int
                             #_{:clj-kondo/ignore [:type-mismatch]}
                             (api/step stmt))]
                  (case code
                    100 (let [result (f ret (column stmt n-cols))]
                          (if (reduced? result)
                            @result
                            (recur result)))
                    101 ret
                    (throw (api/sqlite-ex-info (:pdb conn) code
                             {:sql    (first query)
                              :params (subvec query 1)}))))))))))))

(def default-pragma
  {:cache_size   15625
   :page_size    4096
   :journal_mode "WAL"
   :synchronous  "NORMAL"
   :temp_store   "MEMORY"
   :foreign_keys false
   ;; Because of WAL and a single writer at the application level
   ;; SQLITE_BUSY error should almost never happen, see:
   ;; https://sqlite.org/wal.html#sometimes_queries_return_sqlite_busy_in_wal_mode
   ;; However, they can happen if multiple process access the db
   :busy_timeout 5000
   ;; :optimize cannot be run on connection open when using application
   ;; function in indexes. As you will get a unknown function error.
   ;; https://sqlite.org/pragma.html#pragma_optimize
   ;; :optimize     0x10002
   })

(defn- pragma->set-pragma-query [pragma]
  (conj (->> (merge default-pragma pragma)
          (mapv (fn [[k v]] [(str "pragma " (name k) "=" v)])))))

(defn- new-conn!* [db-name {:keys [pragma read-only]}]
  (let [flags           (if read-only
                          ;; SQLITE_OPEN_READONLY
                          0x00000001
                          ;; SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE
                          (bit-or 0x00000002 0x00000004))
        *pdb            (api/open-v2 db-name flags nil)
        statement-cache (cache/fifo-cache-factory {} :threshold 512)
        conn            {:pdb        *pdb
                         :stmt-cache statement-cache}]
    (->> (pragma->set-pragma-query pragma)
      (run! #(q* conn %)))
    conn))

(defn new-conn!
  [{:keys [name :pragma writer-pragma read-only]}]
  (new-conn!* name
    {:read-only             read-only
     :pragma                (merge pragma writer-pragma)}))

(def ^:dynamic *dbs* nil)

(defn create-write-connections! [dbs]
  (into {} (map (fn [[k opts]] [k (new-conn! opts)])) dbs))

(defn create-read-connections! [dbs]
  (into {} (map (fn [[k opts]] [k (new-conn! (assoc opts :read-only true))]))
    dbs))

(defn start-read-tx [db]
  (q* db ["BEGIN DEFERRED"]))

(defn end-read-tx [db]
  (q* db ["COMMIT"]))

(defmacro with-write-tx
  {:clj-kondo/lint-as 'clojure.core/with-open}
  [[tx db] & body]
  `(let [~tx ~db]
     (try
       (q* ~tx ["BEGIN IMMEDIATE"])
       ~@(butlast body)
       (let [r# ~(last body)]
         (q* ~tx ["COMMIT"])
         r#)
       (catch Throwable t#
         ;; Handles non SQLITE errors crashing a transaction
         (q* ~tx ["ROLLBACK"])
         (throw t#)))))

(defmacro escape-write-tx
  {:clj-kondo/lint-as 'clojure.core/with-open}
  [[tx db] & body]
  `(let [~tx ~db]
     (q* ~tx ["COMMIT"])
     ~@body
     (q* ~tx ["BEGIN IMMEDIATE"])))

(defmacro q [db [query-type query :as string-query] & [params]]
  (if (string? query-type)
    `(q* ~db ~string-query)
    `(q* ~db ~(hsql/format query {:params params}))))

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

;;

(ns hyperlith.impl.sqlite
  (:require [hyperlith.impl.cache :as cache]
            [hyperlith.impl.sqlite.api :as api]
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

(defn- prepare-cached [{:keys [pdb stmt-cache]} query]
  (let [sql    (first query)
        params (subvec query 1)
        stmt   (cache/lookup-or-miss stmt-cache sql
               (fn [_] (api/prepare-v3 pdb sql)))]
    (bind stmt params)
    stmt))

(defmacro with-stmt-reset
  {:clj-kondo/lint-as 'clojure.core/with-open}
  [[stmt-binding stmt] & body]
  `(let [~stmt-binding ~stmt]
     (try
       ~@body
       (finally
         (api/reset ~stmt-binding)
         (api/clear-bindings ~stmt-binding)))))

(defn result-set-reducer [result-set-fn result-set]
  (reduce (fn [result stmt]
            (conj result (result-set-fn stmt)))
    []
    result-set))

(defn q*
  ([conn query]
   (let [stmt (prepare-cached conn query)]
     (with-stmt-reset [stmt stmt]
       (let [code (int
                    #_{:clj-kondo/ignore [:type-mismatch]}
                    (api/step stmt))]
         (case code
           100 nil
           101 nil
           (throw (api/sqlite-ex-info (:pdb conn) code
                    {:sql    (first query)
                     :params (subvec query 1)})))))))
  ([conn query result-set-fn]
   (let [stmt (prepare-cached conn query)]
     (with-stmt-reset [stmt stmt]
       (result-set-reducer result-set-fn
         (reify
           clojure.lang.IReduceInit
           (reduce [_ f init]
             (loop [ret init]
               (let [code (int
                            #_{:clj-kondo/ignore [:type-mismatch]}
                            (api/step stmt))]
                 (case code
                   100 (let [result (f ret stmt)]
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
        statement-cache (cache/init 500)
        conn            {:pdb        *pdb
                         :stmt-cache statement-cache}]
    (->> (pragma->set-pragma-query pragma)
      (run! #(q* conn %)))
    conn))

(defn new-conn!
  [{:keys [name pragma pragma-writer read-only]}]
  (new-conn!* name
    {:read-only read-only
     :pragma    (merge pragma pragma-writer)}))

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

(defmacro q
  [db [query-type query :as string-query] & [a b]]
  (let [params        (when (map? a) a)
        result-set-fn (or (when-not (map? a) a)
                        (when-not (map? b) b))]
    (if (string? query-type)
      (if result-set-fn
        `(q* ~db ~string-query ~result-set-fn)
        `(q* ~db ~string-query))
      (if result-set-fn
        `(q* ~db ~(hsql/format query {:params params}) ~result-set-fn)
        `(q* ~db ~(hsql/format query {:params params}))))))

(def format-query hsql/format)

(def text api/column-text)
(def int api/column-int)
(def blob api/column-blob)
(def real  api/column-double)

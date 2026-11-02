(ns hyperlith.impl.sqlite
  (:require
   [clojure.set :as set]
   [honey.sql :as hsql]
   [hyperlith.impl.cache :as cache]
   [clojure.string :as str])
  (:import
   [clojure.lang Volatile]
   [hyperlith.impl.sqlite Sqlite]
   [java.lang.foreign
    AddressLayout
    Arena
    MemorySegment
    SegmentAllocator
    ValueLayout]))

(set! *warn-on-reflection* true)

(defrecord Conn [pdb stmt-cache query-disabled])

(def ^AddressLayout PTR ValueLayout/ADDRESS)

(defn errmsg [^MemorySegment pdb]
  (-> (Sqlite/errmsg pdb) (.reinterpret Long/MAX_VALUE) (.getString 0)))

(defn errstr [^long code]
  (-> (Sqlite/errstr (int code)) (.reinterpret Long/MAX_VALUE) (.getString 0)))

(defn sqlite-ok? [code] (= code 0))

(defn sqlite-ex-info [pdb code data]
  (let [code-name (errstr code)
        message   (errmsg pdb)]
    (ex-info (str "SQLite error: " code-name "\n" message)
      (assoc data :code code-name :message message))))

(defn open-v2 [^String filename ^long flags ^String vfs]
  (with-open [arena (Arena/ofConfined)]
    (let [ppdb    (.allocate arena PTR)
          c-fname (.allocateFrom arena filename)
          c-vfs   (when vfs (.allocateFrom arena vfs))
          code    (Sqlite/openV2 c-fname ppdb (int flags)
                    (or c-vfs MemorySegment/NULL))]
      (if (sqlite-ok? code)
        (.get ppdb PTR 0)
        (throw (sqlite-ex-info (.get ppdb PTR 0) code {:filename filename}))))))

(defn prepare-v3 [^MemorySegment pdb ^String sql]
  (with-open [arena (Arena/ofConfined)]
    (let [c-sql   (.allocateFrom arena sql)
          sql-len (int (dec (.byteSize c-sql)))
          ppstmt  (.allocate arena PTR)
          code    (Sqlite/prepareV3 pdb c-sql sql-len
                    (int 0x01) ;; SQLITE_PREPARE_PERSISTENT
                    ppstmt MemorySegment/NULL)]
      (if (sqlite-ok? code)
        (.get ppstmt PTR 0)
        (throw (sqlite-ex-info pdb code {:sql sql}))))))

(defn- encode ^MemorySegment [^bytes blob ^Arena arena]
  (let [b-l     (alength blob)
        segment (SegmentAllocator/.allocate arena b-l)]
    (MemorySegment/copy blob 0 segment ValueLayout/JAVA_BYTE 0 b-l)
    segment))

(defn column-text ^String [^MemorySegment stmt ^long idx]
  (let [ptr (Sqlite/columnText stmt (int idx))]
    (when-not (zero? (.address ptr))
      (-> ptr
        (.reinterpret Long/MAX_VALUE)
        (.getString 0)))))

(defn column-blob ^bytes [^MemorySegment stmt ^long idx]
  (let [ptr  (Sqlite/columnBlob stmt (int idx))
        size (Sqlite/columnBytes stmt (int idx))]
    (.toArray (.reinterpret ptr size) ValueLayout/JAVA_BYTE)))

(let [sqlite-static (MemorySegment/ofAddress 0)

      bind-text
      (fn bind-text [^MemorySegment stmt ^long idx ^String text ^Arena arena]
        (let [c-text  (.allocateFrom arena text)
              n-bytes (int (dec (.byteSize c-text)))]
          (Sqlite/bindText stmt (int idx) c-text n-bytes sqlite-static)))

      bind-blob
      (fn bind-blob [^MemorySegment stmt ^long idx ^bytes blob ^Arena arena]
        (let [seg (encode blob arena)]
          (Sqlite/bindBlob stmt (int idx) seg
            (int (.byteSize seg)) sqlite-static)))

      bind-dispatch
      (fn bind-dispatch [stmt col param arena]
        (cond
          (integer? param) (Sqlite/bindInt64  stmt col param)
          (string? param)  (bind-text         stmt col param arena)
          (double? param)  (Sqlite/bindDouble stmt col param)
          (nil? param)     (Sqlite/bindNull   stmt col)
          :else            (bind-blob         stmt col param arena)))]

  (defn- gen-bind-fn [stmt params-fns]
    (let [n                (count params-fns)
          [a b c d :as ks] (mapv (fn [f] (if (fn? f) (f) (fn [_] f)))
                             params-fns)]
      (case n
        0 (fn [_params _arena] stmt)
        1 (fn [params arena]
            (bind-dispatch stmt 1 (a params) arena)
            stmt)
        2 (fn [params arena]
            (bind-dispatch stmt 1 (a params) arena)
            (bind-dispatch stmt 2 (b params) arena)
            stmt)
        3 (fn [params arena]
            (bind-dispatch stmt 1 (a params) arena)
            (bind-dispatch stmt 2 (b params) arena)
            (bind-dispatch stmt 3 (c params) arena)
            stmt)
        4 (fn [params arena]
            (bind-dispatch stmt 1 (a params) arena)
            (bind-dispatch stmt 2 (b params) arena)
            (bind-dispatch stmt 3 (c params) arena)
            (bind-dispatch stmt 4 (d params) arena)
            stmt)
        (fn [params arena]
          (loop [i 0]
            (when (< i n)
              (bind-dispatch stmt (unchecked-inc i) ((nth ks i) params) arena)
              (recur (unchecked-inc i))))
          stmt)))))

(defn- prepare-cached [^Conn conn query params arena]
  (let [stmt-cache (.stmt-cache conn)
        pdb        (.pdb conn)
        bind-fn    (or (cache/get stmt-cache query)
                     (let [[sql & param-fns] (hsql/format-dsl query
                                               {:params params})
                           stmt              (prepare-v3 pdb sql)
                           bind-fn           (gen-bind-fn stmt param-fns)]
                       (cache/put stmt-cache query bind-fn)
                       bind-fn))]
    (bind-fn params arena)))

(defmacro with-stmt-reset
  {:clj-kondo/lint-as 'clojure.core/with-open}
  [[stmt-binding stmt] & body]
  `(let [~stmt-binding ~stmt]
     (try
       ~@body
       (finally
         (Sqlite/reset ~stmt-binding)
         (Sqlite/clearBindings ~stmt-binding)))))

(defn result-set-reducer [result-set-fn result-set]
  ;; TODO: Might benefit from transient on large collections
  ;; but how large?
  (reduce (fn [result stmt]
            (conj result (result-set-fn stmt)))
    []
    result-set))

(defn q
  ([^Conn conn sql]
   (q conn sql nil))
  ([^Conn conn sql params]
   (if (fn? params)
     (q conn sql nil params)
     (if-let [inside (deref ^Volatile (.query-disabled conn))]
       (throw (ex-info
                (str "q cannot be called inside: " inside "\n")
                {:inside inside}))
       (with-open [arena (Arena/ofConfined)]
         (let [stmt (prepare-cached conn sql params arena)]
           (with-stmt-reset [stmt stmt]
             (let [code (Sqlite/step stmt)]
               (case code
                 100 nil
                 101 nil
                 (throw (sqlite-ex-info (.pdb conn) code
                          {:sql    sql
                           :params params}))))))))))
  ([^Conn conn sql params result-set-fn]
   (if-let [inside (deref ^Volatile (.query-disabled conn))]
     (throw (ex-info
              (str "q cannot be called inside: " inside "\n")
              {:inside inside}))
     (with-open [arena (Arena/ofConfined)]
       (let [stmt (prepare-cached conn sql params arena)]
         (with-stmt-reset [stmt stmt]
           (result-set-reducer result-set-fn
             (reify
               clojure.lang.IReduceInit
               (reduce [_ f init]
                 (loop [ret init]
                   (let [code (Sqlite/step stmt)]
                     (case code
                       100 (let [result (f ret stmt)]
                             (if (reduced? result)
                               @result
                               (recur result)))
                       101 ret
                       (throw (sqlite-ex-info (.pdb conn) code
                                {:sql    sql
                                 :params params}))))))))))))))

(def default-pragma
  {:cache_size         15625
   :page_size          4096
   :journal_mode       "WAL"
   :synchronous        "NORMAL"
   :temp_store         "MEMORY"
   :foreign_keys       false
   ;; We own the wal checkpoint
   :wal_autocheckpoint 0
   ;; Because of WAL and a single writer at the application level
   ;; SQLITE_BUSY error should almost never happen, see:
   ;; https://sqlite.org/wal.html#sometimes_queries_return_sqlite_busy_in_wal_mode
   ;; However, they can happen if multiple process access the db
   :busy_timeout       5000
   ;; :optimize cannot be run on connection open when using application
   ;; function in indexes. As you will get a unknown function error.
   ;; https://sqlite.org/pragma.html#pragma_optimize
   ;; :optimize     0x10002
   })

(defn- pragma->set-pragma-query [pragma]
  (conj (->> (merge default-pragma pragma)
          (mapv (fn [[k v]] (str "pragma " (name k) "=" v))))))

(defn- new-conn!* [db-name {:keys [pragma read-only query-disabled
                                   max-entries]}]
  (let [flags      (if read-only
                     ;; SQLITE_OPEN_READONLY
                     0x00000001
                     ;; SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE
                     (bit-or 0x00000002 0x00000004))
        *pdb       (open-v2 db-name flags nil)
        stmt-cache (cache/init
                     {:max-entries max-entries                      
                      :executor
                      ;; As each connection has it's own cache maintenance
                      ;; can be done inline.
                      (reify java.util.concurrent.Executor
                        (execute [_ task]
                          (.run ^Runnable task)))})
        conn       (map->Conn
                     {:pdb            *pdb
                      :stmt-cache     stmt-cache
                      :query-disabled query-disabled})]
    (->> (pragma->set-pragma-query pragma)
      (run! #(q conn {:raw %})))
    conn))

(defn new-conn!
  [{:keys [name pragma pragma-writer read-only query-disabled max-entries]}]
  (new-conn!* name
    {:max-entries    (or max-entries 500)
     :query-disabled (or query-disabled (volatile! nil))
     :read-only      read-only
     :pragma         (merge pragma pragma-writer)}))

(defn start-read-tx [db]
  (q db {:raw "BEGIN DEFERRED"}))

(defn end-read-tx [db]
  (q db {:raw "COMMIT"}))

(defmacro with-write-tx
  {:clj-kondo/lint-as 'clojure.core/with-open}
  [[tx db] & body]
  `(let [~tx ~db]
     (try
       (q ~tx {:raw "BEGIN IMMEDIATE"})
       ~@(butlast body)
       (let [r# ~(last body)]
         (q ~tx {:raw "COMMIT"})
         r#)
       (catch Throwable t#
         ;; Handles non SQLITE errors crashing a transaction
         (q ~tx {:raw "ROLLBACK"})
         (throw t#)))))

(defmacro escape-write-tx
  {:clj-kondo/lint-as 'clojure.core/with-open}
  [[tx db] & body]
  `(let [~tx ~db]
     (q ~tx {:raw "COMMIT"})
     ~@body
     (q ~tx {:raw "BEGIN IMMEDIATE"})))

(defn create-write-connections! [dbs]
  (into {}
    (map (fn [[k {:keys [migrations] :as opts}]]
           (let [conn (new-conn! opts)]
             (when migrations (migrations conn))
             [k conn]))) dbs))

(defn create-read-connections! [dbs query-disabled]
  (into {} (map (fn [[k opts]]
                  [k (new-conn!
                       (assoc opts
                         :read-only true
                         :query-disabled query-disabled))]))
    dbs))

(defn checkpoint-all-dbs! [dbs]
  (run!
    (fn [db] (q db {:raw "PRAGMA wal_checkpoint(TRUNCATE)"}))
    (vals dbs)))

(def format-query hsql/format)

(def text column-text)
(def blob column-blob)
(defn real ^double [^MemorySegment stmt ^long idx]
  (Sqlite/columnDouble stmt (int idx)))
(defn int ^long [^MemorySegment stmt ^long idx]
  (Sqlite/columnInt64 stmt (clojure.core/int idx)))
(defn any [^MemorySegment stmt ^long n]
  (case (Sqlite/columnType stmt n)
    ;; See type codes here: https://sqlite.org/c3ref/c_blob.html
    1 (Sqlite/columnInt64    stmt n)
    2 (Sqlite/columnDouble stmt n)
    3 (column-text   stmt n)
    4 (column-blob   stmt n)
    5 nil))

(defn table-list [db]
  (q db '{select [name] from pragma_table_list}
    (fn [stmt] [(text stmt 0)])))

(defn- keyword-kebab [s]
  (keyword (str/replace (name s) "_" "-")))

(defn table-columns [db table]
  (q db '{select [name] from [[[pragma_table_info ?table]]]}
    {:table (name table)}
    (fn [stmt] (keyword-kebab (text stmt 0)))))

(defn add-column! [db table column]
  (q db {:alter-table (keyword-kebab (name table))
         :add-column  [(keyword-kebab column)]}))

(defn ensure-columns!
  "Adds columns to a table if they are missing. The columns will have no
  type affinity. This is useful for sparse tables."
  [db table columns]
  (run!
    (fn [column] (add-column! db table column))
    (set/difference
      (into #{} (map keyword-kebab columns))
      (into #{} (table-columns db table)))))

(comment
  (hsql/format
    '{select [data]
      from   session
      where  [= id ?sid]
      limit  1}
    {:params {:sid 3}}))

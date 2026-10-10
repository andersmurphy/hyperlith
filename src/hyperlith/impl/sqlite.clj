(ns hyperlith.impl.sqlite
  (:require [honey.sql :as hsql])
  (:import [java.util HashMap]
           [hyperlith.impl.sqlite Sqlite]
           [java.lang.foreign Arena AddressLayout MemorySegment
            SegmentAllocator ValueLayout]))

(set! *warn-on-reflection* true)

(def ^AddressLayout PTR ValueLayout/ADDRESS)

(defn- str->seg ^MemorySegment [^Arena arena ^String s]
  (.allocateFrom arena s))

(def ^MemorySegment sqlite-static    (MemorySegment/ofAddress 0))
(def ^MemorySegment sqlite-transient (MemorySegment/ofAddress -1))

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

(defn open-v2 [^String filename ^long flags vfs]
  (with-open [arena (Arena/ofConfined)]
    (let [ppdb    (.allocate arena PTR)
          c-fname (str->seg arena filename)
          c-vfs   (when vfs (str->seg arena vfs))
          code    (Sqlite/openV2 c-fname ppdb (int flags)
                    (or c-vfs MemorySegment/NULL))]
      (if (sqlite-ok? code)
        (.get ppdb PTR 0)
        (throw (sqlite-ex-info (.get ppdb PTR 0) code {:filename filename}))))))

(defn prepare-v3 [^MemorySegment pdb ^String sql]
  (with-open [arena (Arena/ofConfined)]
    (let [c-sql   (str->seg arena sql)
          sql-len (int (dec (.byteSize c-sql)))
          ppstmt  (.allocate arena PTR)
          code    (Sqlite/prepareV3 pdb c-sql sql-len
                    (int 0x01) ;; SQLITE_PREPARE_PERSISTENT
                    ppstmt MemorySegment/NULL)]
      (if (sqlite-ok? code)
        (.get ppstmt PTR 0)
        (throw (sqlite-ex-info pdb code {:sql sql}))))))

(defn bind-text [^MemorySegment stmt ^long idx text]
  (with-open [arena (Arena/ofConfined)]
    (let [c-text  (str->seg arena (str text))
          n-bytes (int (dec (.byteSize c-text)))]
      (Sqlite/bindText stmt (int idx) c-text n-bytes sqlite-transient))))

(defn- encode ^MemorySegment [^Arena arena ^bytes blob]
  (let [b-l     (alength blob)
        segment (SegmentAllocator/.allocate arena b-l)]
    (MemorySegment/copy blob 0 segment ValueLayout/JAVA_BYTE 0 b-l)
    segment))

(defn bind-blob [^MemorySegment stmt ^long idx ^bytes blob]
  (with-open [arena (Arena/ofConfined)]
    (let [seg (encode arena blob)]
      (Sqlite/bindBlob stmt (int idx) seg (int (.byteSize seg)) sqlite-transient))))

(defn column-text [^MemorySegment stmt ^long idx]
  (-> (Sqlite/columnText stmt (int idx)) (.reinterpret Long/MAX_VALUE)
    (.getString 0)))

(defn column-blob [^MemorySegment stmt ^long idx]
  (let [ptr  (Sqlite/columnBlob stmt (int idx))
        size (Sqlite/columnBytes stmt idx)]
    (.toArray (.reinterpret ptr size) ValueLayout/JAVA_BYTE)))

(defn- bind [stmt params]
  (reduce
    (fn [i param]
      (cond
        (integer? param) (Sqlite/bindInt64  stmt i param)
        (double? param)  (Sqlite/bindDouble stmt i param)
        (string? param)  (bind-text         stmt i param)
        (nil? param)     (Sqlite/bindNull   stmt i)
        :else            (bind-blob         stmt i param))
      (inc i))
    1 ;; starts at 1
    params))

(defn- prepare-cached [{:keys [pdb ^HashMap stmt-cache]} sql params]
  (let [stmt (or (HashMap/.get stmt-cache sql)
               (let [stmt (prepare-v3 pdb sql)]
                 (HashMap/.put stmt-cache sql stmt)
                 stmt))]
    (bind stmt params)
    stmt))

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
  (let [result
        (reduce (fn [result stmt]
                  (conj result (result-set-fn stmt)))
          []
          result-set)]
    (when (first result) result)))

(defn q*
  ([conn sql]
   (q* conn sql nil))
  ([conn sql params]
   (if-let [inside @(:query-disabled conn)]
     (throw (ex-info
              (str "q cannot be called inside: " inside "\n")
              {:inside inside}))
     (let [stmt (prepare-cached conn sql params)]
       (with-stmt-reset [stmt stmt]
         (let [code (int
                      #_{:clj-kondo/ignore [:type-mismatch]}
                      (Sqlite/step stmt))]
           (case code
             100 nil
             101 nil
             (throw (sqlite-ex-info (:pdb conn) code
                      {:sql    sql
                       :params params}))))))))
  ([conn sql params result-set-fn]
   (if-let [inside @(:query-disabled conn)]
     (throw (ex-info
              (str "q cannot be called inside: " inside "\n")
              {:inside inside}))
     (let [stmt (prepare-cached conn sql params)]
       (with-stmt-reset [stmt stmt]
         (result-set-reducer result-set-fn
           (reify
             clojure.lang.IReduceInit
             (reduce [_ f init]
               (loop [ret init]
                 (let [code (int
                              #_{:clj-kondo/ignore [:type-mismatch]}
                              (Sqlite/step stmt))]
                   (case code
                     100 (let [result (f ret stmt)]
                           (if (reduced? result)
                             @result
                             (recur result)))
                     101 ret
                     (throw (sqlite-ex-info (:pdb conn) code
                              {:sql    sql
                               :params params})))))))))))))

(def default-pragma
  {:cache_size   15625
   :page_size    4096
   :journal_mode "WAL"
   :synchronous  "NORMAL"
   :temp_store   "MEMORY"
   :foreign_keys false
   ;; We own the wal checkpoint
   :wal_autocheckpoint 0
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
          (mapv (fn [[k v]] (str "pragma " (name k) "=" v))))))

(defn- new-conn!* [db-name {:keys [pragma read-only query-disabled]}]
  (let [flags      (if read-only
                          ;; SQLITE_OPEN_READONLY
                          0x00000001
                          ;; SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE
                          (bit-or 0x00000002 0x00000004))
        *pdb       (open-v2 db-name flags nil)
        stmt-cache (HashMap.)
        conn       {:pdb        *pdb
                    :stmt-cache stmt-cache
                    :query-disabled query-disabled}]
    (->> (pragma->set-pragma-query pragma)
      (run! #(q* conn %)))
    conn))

(defn new-conn!
  [{:keys [name pragma pragma-writer read-only query-disabled]}]
  (new-conn!* name
    {:query-disabled (or query-disabled (atom nil))
     :read-only        read-only
     :pragma           (merge pragma pragma-writer)}))

(def ^:dynamic *dbs* nil)

(defn create-write-connections! [dbs]
  (into {} (map (fn [[k opts]] [k (new-conn! opts)])) dbs))

(defn create-read-connections! [dbs query-disabled]
  (into {} (map (fn [[k opts]]
                  [k (new-conn!
                       (assoc opts
                         :read-only true
                         :query-disabled query-disabled))]))
    dbs))

(defn start-read-tx [db]
  (q* db "BEGIN DEFERRED"))

(defn end-read-tx [db]
  (q* db "COMMIT"))

(defmacro with-write-tx
  {:clj-kondo/lint-as 'clojure.core/with-open}
  [[tx db] & body]
  `(let [~tx ~db]
     (try
       (q* ~tx "BEGIN IMMEDIATE")
       ~@(butlast body)
       (let [r# ~(last body)]
         (q* ~tx "COMMIT")
         r#)
       (catch Throwable t#
         ;; Handles non SQLITE errors crashing a transaction
         (q* ~tx "ROLLBACK")
         (throw t#)))))

(defmacro escape-write-tx
  {:clj-kondo/lint-as 'clojure.core/with-open}
  [[tx db] & body]
  `(let [~tx ~db]
     (q* ~tx "COMMIT")
     ~@body
     (q* ~tx "BEGIN IMMEDIATE")))

(defmacro q
  [db [query-type query :as string-query] & [a b]]
  (let [params         (when (map? a) a)
        result-set-fn  (or (when-not (map? a) a)
                         (when-not (map? b) b))
        [sql & params] (if (string? query-type)
                         string-query
                         (hsql/format query {:params params}))]
    (if result-set-fn
      `(q* ~db ~sql ~(vec params) ~result-set-fn)
      `(q* ~db ~sql ~(vec params)))))

(def format-query hsql/format)

(def text column-text)
(def blob column-blob)
(defn real [^MemorySegment stmt ^long idx]
  (Sqlite/columnDouble stmt (int idx)))
(defn int [^MemorySegment stmt ^long idx]
  (Sqlite/columnInt64 stmt (clojure.core/int idx)))

(comment
  (hsql/format
    '{select [data]
      from   session
      where  [= id ?sid]
      limit  1}
    {:params {:sid 3}})
  
  (macroexpand
    '(q db
       '{select [data]
         from   session
         where  [= id ?sid]
         limit  1}
       {:sid 1}
       (fn []))))

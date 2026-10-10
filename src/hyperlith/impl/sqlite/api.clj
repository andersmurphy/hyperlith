(ns hyperlith.impl.sqlite.api
  (:require
   [clojure.java.io :as io]
   [clojure.string  :as str])
  (:import
   [java.lang.foreign
    AddressLayout
    Arena
    FunctionDescriptor
    Linker
    MemoryLayout
    MemorySegment
    SegmentAllocator
    SymbolLookup
    ValueLayout]
   [java.lang.invoke MethodHandle]
   [java.nio.file Files]
   [java.nio.file.attribute FileAttribute]))

(set! *warn-on-reflection* true)

(def ^Linker linker (Linker/nativeLinker))

(def ^Arena global-arena (Arena/global))

(defn copy-resource [resource-path output-path]
  (with-open [in  (io/input-stream (io/resource resource-path))
              out (io/output-stream (io/file output-path))]
    (io/copy in out)))

(defn get-arch+os []
  (let [os-name (str/lower-case (System/getProperty "os.name"))]
    (str (System/getProperty "os.arch") "-"
      (cond (str/includes? os-name "win") "windows"
            (str/includes? os-name "nux") "linux"
            (str/includes? os-name "mac") "macos"))))

(defn load-bundled-library ^SymbolLookup []
  (let [res-file
        (case (get-arch+os)
          "aarch64-linux"   "sqlite3_aarch64-linux-gnu.so"
          "aarch64-macos"   "sqlite3_aarch64-macos-none.so"
          ("x86-linux"
           "x86_64-linux"
           "amd64-linux")   "sqlite3_x86_64-linux-gnu.so"
          ("x86-macos"
           "x86_64-macos"
           "amd64-macos")   "sqlite3_x86_64-macos-none.so"
          ("x86-windows"
           "x86_64-windows"
           "amd64-windows") "sqlite3_x86_64-windows-gnu.dll")
        temp-lib (Files/createTempFile "sqlite4clj_" (str "_" res-file)
                   (make-array FileAttribute 0))]
    (try
      (copy-resource res-file (str temp-lib))
      (SymbolLookup/libraryLookup (str temp-lib) global-arena)
      (finally
        (Files/deleteIfExists temp-lib)))))

(defonce ^SymbolLookup sqlite-lookup
  (load-bundled-library))

(defn- find-sym ^MemorySegment [name]
  (-> sqlite-lookup (.find name) (.orElseThrow)))

(defn- method-handle ^MethodHandle
  ([sym-name args ret]
   (.downcallHandle linker (find-sym sym-name)
     (FunctionDescriptor/of ret
       (into-array MemoryLayout args))
     (into-array java.lang.foreign.Linker$Option [])))
  ([sym-name args]
   (.downcallHandle linker (find-sym sym-name)
     (FunctionDescriptor/ofVoid
       (into-array MemoryLayout args))
     (into-array java.lang.foreign.Linker$Option []))))

(def ^AddressLayout PTR ValueLayout/ADDRESS)

(defn- str->seg ^MemorySegment [^Arena arena ^String s]
  (.allocateFrom arena s))

(def ^MemorySegment sqlite-static    (MemorySegment/ofAddress 0))
(def ^MemorySegment sqlite-transient (MemorySegment/ofAddress -1))

(defn invoke ^MemorySegment [^MethodHandle mh & args]
  (.invokeWithArguments mh (object-array args)))

(let [mh (method-handle "sqlite3_initialize" [] ValueLayout/JAVA_INT)]
  (defn initialize ^long [] (invoke mh)))

(defonce ^:private _init (initialize))

(let [mh (method-handle "sqlite3_free" [PTR])]
  (defn free [^MemorySegment p]
    (invoke mh (object-array [p]))))

(let [mh (method-handle "sqlite3_errmsg" [PTR] PTR)]
  (defn errmsg [^MemorySegment pdb]
    (->  (invoke mh pdb)
      (.reinterpret Long/MAX_VALUE)
      (.getString 0))))

(let [mh (method-handle "sqlite3_errstr" [ValueLayout/JAVA_INT] PTR)]
  (defn errstr [^long code]
    (-> ^MemorySegment (invoke mh (int code))
      (.reinterpret Long/MAX_VALUE)
      (.getString 0))))

(defn sqlite-ok? [code] (= code 0))

(defn sqlite-ex-info [pdb code data]
  (let [code-name (errstr code)
        message   (errmsg pdb)]
    (ex-info (str "SQLite error: " code-name "\n" message)
      (assoc data :code code-name :message message))))

(let [mh (method-handle "sqlite3_open_v2"
           [PTR PTR ValueLayout/JAVA_INT PTR] ValueLayout/JAVA_INT)]
  (defn open-v2
    [^String filename ^long flags vfs]
    (with-open [arena (Arena/ofConfined)]
      (let [ppdb    (.allocate arena PTR)
            c-fname (str->seg arena filename)
            c-vfs   (when vfs (str->seg arena vfs))
            code    (int (invoke mh c-fname ppdb (int flags)
                           (or c-vfs MemorySegment/NULL)))]
        (if (sqlite-ok? code)
          (.get ppdb PTR 0)
          (throw (sqlite-ex-info (.get ppdb PTR 0)
                   code {:filename filename})))))))

(let [mh (method-handle "sqlite3_close" [PTR] ValueLayout/JAVA_INT)]
  (defn close [^MemorySegment pdb]
    (invoke mh pdb)))

(let [mh (method-handle "sqlite3_prepare_v3"
           [PTR PTR ValueLayout/JAVA_INT ValueLayout/JAVA_INT PTR PTR]
           ValueLayout/JAVA_INT)]
  (defn prepare-v3
    [^MemorySegment pdb ^String sql]
    (with-open [arena (Arena/ofConfined)]
      (let [c-sql   (str->seg arena sql)
            sql-len (int (dec (.byteSize c-sql)))
            ppstmt  (.allocate arena PTR)
            code    (int (invoke mh
                           pdb c-sql sql-len
                           (int 0x01) ;; SQLITE_PREPARE_PERSISTENT
                           ppstmt
                           MemorySegment/NULL))]
        (if (sqlite-ok? code)
          (.get ppstmt PTR 0)
          (throw (sqlite-ex-info pdb code {:sql sql})))))))

(let [mh (method-handle "sqlite3_reset" [PTR]
           ValueLayout/JAVA_INT)]
  (defn reset [^MemorySegment stmt]
    (invoke mh stmt)))

(let [mh (method-handle "sqlite3_clear_bindings" [PTR] ValueLayout/JAVA_INT)]
  (defn clear-bindings [^MemorySegment stmt]
    (invoke mh stmt)))

(let [mh (method-handle "sqlite3_bind_int64"
           [PTR ValueLayout/JAVA_INT ValueLayout/JAVA_LONG]
           ValueLayout/JAVA_INT)]
  (defn bind-int [^MemorySegment stmt ^long idx ^long v]
    (invoke mh stmt (int idx) v)))

(let [mh (method-handle "sqlite3_bind_double"
           [PTR ValueLayout/JAVA_INT ValueLayout/JAVA_DOUBLE]
           ValueLayout/JAVA_INT)]
  (defn bind-double [^MemorySegment stmt ^long idx ^double v]
    (invoke mh stmt (int idx) v)))

(let [mh (method-handle "sqlite3_bind_null"
           [PTR ValueLayout/JAVA_INT]
           ValueLayout/JAVA_INT)]
  (defn bind-null [^MemorySegment stmt ^long idx]
    (invoke mh stmt (int idx))))

(let [mh (method-handle "sqlite3_bind_text"
           [PTR ValueLayout/JAVA_INT PTR ValueLayout/JAVA_INT PTR]
           ValueLayout/JAVA_INT)]
  (defn bind-text [^MemorySegment stmt ^long idx text]
    (with-open [arena (Arena/ofConfined)]
      (let [s       (str text)
            c-text  (str->seg arena s)
            ;; byte length excluding the null terminator
            n-bytes (int (dec (.byteSize c-text)))]
        (invoke mh
          stmt (int idx) c-text n-bytes sqlite-transient)))))

(defn- encode
  ^MemorySegment [^Arena arena ^bytes blob]
  (let [b-l     (alength blob)
        segment (SegmentAllocator/.allocate arena b-l)]
    (MemorySegment/copy blob 0 segment ValueLayout/JAVA_BYTE 0 b-l)
    segment))

(let [mh (method-handle "sqlite3_bind_blob"
           [PTR ValueLayout/JAVA_INT PTR ValueLayout/JAVA_INT PTR]
           ValueLayout/JAVA_INT)]
  (defn bind-blob [^MemorySegment stmt ^long idx ^bytes blob]
    (with-open [arena (Arena/ofConfined)]
      (let [seg  (encode arena blob)
            size (int (.byteSize seg))]
        (invoke mh stmt (int idx) seg size sqlite-transient)))))

(let [mh (method-handle "sqlite3_step"
           [PTR]
           ValueLayout/JAVA_INT)]
  (defn step [^MemorySegment stmt]
    (invoke mh stmt)))

(let [mh (method-handle "sqlite3_column_count"
           [PTR]
           ValueLayout/JAVA_INT)]
  (defn column-count [^MemorySegment stmt]
    (invoke mh stmt)))

(let [mh (method-handle "sqlite3_column_double"
           [PTR ValueLayout/JAVA_INT]
           ValueLayout/JAVA_DOUBLE)]
  (defn column-double [^MemorySegment stmt ^long idx]
    (invoke mh stmt (int idx))))

(let [mh (method-handle "sqlite3_column_int64"
           [PTR ValueLayout/JAVA_INT]
           ValueLayout/JAVA_LONG)]
  (defn column-int [^MemorySegment stmt ^long idx]
    (invoke mh stmt (int idx))))

(let [mh (method-handle "sqlite3_column_text"
           [PTR ValueLayout/JAVA_INT]
           PTR)]
  (defn column-text [^MemorySegment stmt ^long idx]
    (-> ^MemorySegment (invoke mh stmt (int idx))
      (.reinterpret Long/MAX_VALUE)
      (.getString 0))))

(let [mh (method-handle "sqlite3_column_bytes"
           [PTR ValueLayout/JAVA_INT]
           ValueLayout/JAVA_INT)]
  (defn column-bytes [^MemorySegment stmt ^long idx]
    (invoke mh stmt (int idx))))

(let [mh (method-handle "sqlite3_column_blob"
           [PTR ValueLayout/JAVA_INT]
           PTR)]
  (defn column-blob [^MemorySegment stmt ^long idx]
    (let [ptr  ^MemorySegment (invoke mh stmt (int idx))
          size (column-bytes stmt idx)
          seg  (.reinterpret ptr size)]
      (.toArray seg ValueLayout/JAVA_BYTE))))

(let [mh (method-handle "sqlite3_column_type"
           [PTR ValueLayout/JAVA_INT]
           ValueLayout/JAVA_INT)]
  (defn column-type [^MemorySegment stmt ^long idx]
    (invoke mh stmt (int idx))))

(let [mh (method-handle "sqlite3_finalize"
           [PTR]
           ValueLayout/JAVA_INT)]
  (defn finalize [^MemorySegment stmt]
    (invoke mh stmt)))

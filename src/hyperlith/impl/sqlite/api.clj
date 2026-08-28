(ns hyperlith.impl.sqlite.api
  "These function map directly to SQLite's C API."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [coffi.ffi :as ffi]
   [coffi.mem :as mem]
   [hyperlith.impl.sqlite.ffi-wrapper :as ffi-wrapper :refer [defcfn]])
  (:import
   [java.nio.file Files]
   [java.lang.foreign MemorySegment SegmentAllocator]))

(set! *warn-on-reflection* true)

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

(defn load-bundled-library []
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
        temp-lib-filename (str "sqlite4clj_temp_" res-file)]
    (copy-resource res-file temp-lib-filename)
    (ffi-wrapper/set-library! temp-lib-filename)
    ;; We delete once loaded
    (Files/deleteIfExists (.toPath (io/file temp-lib-filename)))))

(defn load-system-library []
  (ffi/load-system-library "sqlite3"))

;; Load appropriate SQLite library
(let [src (System/getProperty "sqlite4clj.native-lib")]
  (cond
    ;; default to bundled
    (or (nil? src)
      (= src "bundled")) (load-bundled-library)
    (= src "system")     (load-system-library)
    :else
    (ffi-wrapper/set-library! src)))

(defcfn initialize
  sqlite3_initialize [] ::mem/int)

(defonce init-lib
  (initialize))

(defcfn free
  sqlite3_free
  [::mem/pointer] ::mem/void)

(defcfn errmsg
  sqlite3_errmsg
  [::mem/pointer] ::mem/c-string)

(defcfn errstr
  sqlite3_errstr
  [::mem/int] ::mem/c-string)

(defn sqlite-ex-info [pdb code data]
  (let [code-name (errstr code)
        message   (errmsg pdb)]
    (ex-info (str "SQLite error: " code-name "\n" message)
      (assoc data
        :code code-name
        :message message))))

(defn sqlite-ok? [code]
  (= code 0))

(defcfn open-v2
  "sqlite3_open_v2" [::mem/c-string ::mem/pointer ::mem/int
                     ::mem/c-string] ::mem/int
  sqlite3-open-native
  [filename flags vfs]
  (with-open [arena (mem/confined-arena)]
    (let [pdb           (mem/alloc-instance ::mem/pointer arena)
          filename-utf8 (String/new (String/.getBytes filename "UTF-8") "UTF-8")
          vfs-utf8      (when vfs
                          (String/new (String/.getBytes vfs "UTF-8") "UTF-8"))
          code          (sqlite3-open-native filename-utf8
                          pdb flags vfs-utf8)]
      (if (sqlite-ok? code)
        (mem/deserialize-from pdb ::mem/pointer)
        (throw (sqlite-ex-info pdb code {:filename filename}))))))

(defcfn close
  sqlite3_close
  [::mem/pointer] ::mem/int)

(defcfn prepare-v3
  "sqlite3_prepare_v3"
  [::mem/pointer ::mem/c-string ::mem/int
   ::mem/int
   ::mem/pointer ::mem/pointer] ::mem/int
  sqlite3-prepare-native
  [pdb sql]
  (with-open [arena (mem/confined-arena)]
    (let [ppStmt (mem/alloc-instance ::mem/pointer arena)
          sql    (String/new (String/.getBytes sql "UTF-8") "UTF-8")
          code   (sqlite3-prepare-native pdb sql -1
                   0x01 ;; SQLITE_PREPARE_PERSISTENT
                   ppStmt
                   nil)]
      (if (sqlite-ok? code)
        (mem/deserialize-from ppStmt ::mem/pointer)
        (throw (sqlite-ex-info pdb code {:sql sql}))))))

(defcfn reset
  sqlite3_reset
  [::mem/pointer] ::mem/int)

(defcfn clear-bindings
  sqlite3_clear_bindings
  [::mem/pointer] ::mem/int)

(defcfn bind-int
  sqlite3_bind_int64
  [::mem/pointer ::mem/int ::mem/long] ::mem/int)

(defcfn bind-double
  sqlite3_bind_double
  [::mem/pointer ::mem/int ::mem/double] ::mem/int)

(defcfn bind-null
  sqlite3_bind_null
  [::mem/pointer ::mem/int] ::mem/int)

(def sqlite-static (mem/as-segment 0))
(def sqlite-transient (mem/as-segment -1))

(defcfn bind-text
  "sqlite3_bind_text"
  [::mem/pointer ::mem/int ::mem/c-string ::mem/int
   ::mem/pointer] ::mem/int
  sqlite3-bind-text-native
  [pdb idx text]
  (let [text       (str text)
        text-bytes (String/.getBytes text "UTF-8")]
    (sqlite3-bind-text-native pdb idx
      (String/new text-bytes "UTF-8")
      (count text-bytes)
      sqlite-transient)))

(defn encode ^MemorySegment [arena blob]
  (let [b-l     (alength ^bytes blob)
        segment (SegmentAllocator/.allocate arena b-l)]
    (mem/write-bytes segment b-l 0 ^bytes blob)
    segment))

(defcfn bind-blob
  "sqlite3_bind_blob"
  [::mem/pointer ::mem/int ::mem/pointer ::mem/int
   ::mem/pointer] ::mem/int
  sqlite3-bind-blob-native
  [pdb idx blob]
  (with-open [arena (mem/confined-arena)]
    (let [segment (encode arena blob)]
      (sqlite3-bind-blob-native pdb idx segment
        (MemorySegment/.byteSize segment)
        sqlite-transient))))

(defcfn step
  sqlite3_step
  [::mem/pointer] ::mem/int)

(defcfn column-count
  sqlite3_column_count
  [::mem/pointer] ::mem/int)

(defcfn column-double
  sqlite3_column_double
  [::mem/pointer ::mem/int] ::mem/double)

(defcfn column-int
  sqlite3_column_int64
  [::mem/pointer ::mem/int] ::mem/long)

(defcfn column-text
  sqlite3_column_text
  [::mem/pointer ::mem/int] ::mem/c-string)

(defcfn column-bytes
  sqlite3_column_bytes
  [::mem/pointer ::mem/int] ::mem/int)

(defcfn column-blob
  "sqlite3_column_blob"
  [::mem/pointer ::mem/int] ::mem/pointer
  sqlite3_column_blob-native
  [stmt idx]
  (with-open [arena (mem/confined-arena)]
    (let [result (sqlite3_column_blob-native stmt idx)
          size   (column-bytes stmt idx)
          blob   (mem/reinterpret result size arena)]
      (.toArray blob java.lang.foreign.ValueLayout/JAVA_BYTE))))

(defcfn column-type
  sqlite3_column_type
  [::mem/pointer ::mem/int] ::mem/int)

(defcfn finalize
  sqlite3_finalize
  [::mem/pointer] ::mem/int)


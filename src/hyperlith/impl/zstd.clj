(ns hyperlith.impl.zstd
  (:import (com.github.luben.zstd Zstd ZstdOutputStreamNoFinalizer
             ZstdCompressCtx)
           (java.io ByteArrayOutputStream OutputStream)))

(defn compress ^byte/1 [data level]
  ;; Browser spec only support up to 8MB window which means
  ;; level 19 is the max we can use. See RFC 9659.
  (assert (<= 1 level 19)) 
  (let [^bytes data (if (string? data) (String/.getBytes data) data)]
    (with-open [ctx (ZstdCompressCtx/new)]
      (.setLevel ctx (int level))
      (.compress ctx data))))

(defn decompress ^byte/1 [^byte/1 data]
  (Zstd/decompress data (int (Zstd/decompressedSize data))))

(defn compress-out-stream ^OutputStream
  [^ByteArrayOutputStream out level window]
  (-> (ZstdOutputStreamNoFinalizer/new out)
    (.setLevel (int level))
    (.setWindowLog window)
    ;; skip frame checksum (TLS covers integrity)
    (.setChecksum false)
    ;; keep block open across flushes (better ratio)
    (.setCloseFrameOnFlush false)))

(ns hyperlith.impl.zstd
  (:import (com.github.luben.zstd Zstd ZstdOutputStreamNoFinalizer
             ZstdCompressCtx)
           (java.io ByteArrayOutputStream OutputStream)))

(defn compress ^byte/1 [data level]
  (let [^bytes data (if (string? data) (String/.getBytes data) data)]
    (with-open [ctx (ZstdCompressCtx/new)]
      (.setLevel ctx (int level))
      (.compress ctx data))))

(defn decompress ^byte/1 [^byte/1 data]
  (Zstd/decompress data (int (Zstd/decompressedSize data))))

(defn compress-out-stream ^OutputStream
  [^ByteArrayOutputStream out-stream level]
  (ZstdOutputStreamNoFinalizer/new out-stream (int level)))

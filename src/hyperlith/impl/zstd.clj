(ns hyperlith.impl.zstd
  (:import
   (com.github.luben.zstd EndDirective Zstd ZstdCompressCtx)
   [io.netty.buffer ByteBuf PooledByteBufAllocator]
   [java.nio ByteBuffer]))

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

(defn ctx ^ZstdCompressCtx [level window]
  (-> (ZstdCompressCtx.)
    (.setLevel (int level))
    (.setWindowLog (int window))
    (.setChecksum false)))

(defn close-ctx [^ZstdCompressCtx ctx]
  (.close ctx))

(defn buf->nio ^ByteBuffer [^ByteBuf buf]
  (doto (.nioBuffer buf 0 (.capacity buf))
    (.limit (.capacity buf)) (.position 0)))

(defn compress-chunk! ^ByteBuffer
  [^ZstdCompressCtx ctx ^ByteBuffer src]
  (let [dst     ^ByteBuf (.directBuffer PooledByteBufAllocator/DEFAULT 32768 32768)
        dst-nio (buf->nio dst)]
    (.flip src)
    (.compressDirectByteBufferStream ctx dst-nio src EndDirective/CONTINUE)
    (.writerIndex dst (.position dst-nio))
    (.clear src)
    dst))

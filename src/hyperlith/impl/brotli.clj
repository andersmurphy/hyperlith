(ns hyperlith.impl.brotli
  (:import (com.aayushatharva.brotli4j Brotli4jLoader)
           (com.aayushatharva.brotli4j.encoder Encoder Encoder$Parameters
             Encoder$Mode BrotliOutputStream)
           (com.aayushatharva.brotli4j.decoder Decoder)
           (java.io ByteArrayOutputStream)))

#_:clj-kondo/ignore
(defonce ensure-br
  (Brotli4jLoader/ensureAvailability))

(defn encoder-params [{:keys [quality]}]
  (doto (Encoder$Parameters/new)
    (.setMode Encoder$Mode/TEXT)
    (.setQuality (or quality 5))))

(defn compress [data & {:as opts}]
  (-> (if (string? data) (String/.getBytes data) ^byte/1 data)
    (Encoder/compress (encoder-params opts))))

(defn byte-array-out-stream ^ByteArrayOutputStream []
  (ByteArrayOutputStream/new))

(defn compress-out-stream ^BrotliOutputStream
  [^ByteArrayOutputStream out-stream & {:as opts}]
  (BrotliOutputStream/new out-stream (encoder-params opts)))

(defn compress-chunk [^ByteArrayOutputStream out ^BrotliOutputStream br chunk]
  (doto br
    (.write  (String/.getBytes chunk))
    (.flush))
  (let [result (.toByteArray out)]
    (.reset out)
    result))

(defn decompress [data]
  (let [decompressed (Decoder/decompress data)]
    (String/new (.getDecompressedData decompressed))))




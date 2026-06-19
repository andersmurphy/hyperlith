(ns hyperlith.impl.brotli
  (:require [clojure.math :as m])
  (:import (com.aayushatharva.brotli4j Brotli4jLoader )
           (com.aayushatharva.brotli4j.decoder Decoder)
           (com.aayushatharva.brotli4j.encoder
             BrotliOutputStream
             Encoder
             Encoder$Mode
             Encoder$Parameters)
           (java.io ByteArrayOutputStream OutputStream)))

#_:clj-kondo/ignore
(defonce ensure-br
  (Brotli4jLoader/ensureAvailability))

(defn window-size->kb [window-size]
  (/ (- (m/pow 2 window-size) 16) 1000))

(defn encoder-params [opts]
  (doto (Encoder$Parameters/new)
    (.setMode Encoder$Mode/TEXT)
    ;; LZ77 window size (0, 10-24) (default: 24)
    ;; window size is (pow(2, NUM) - 16)
    (.setQuality (or (opts :quality) 3))
    (.setWindow  (or (opts :window-size) 22))))

(defn compress
  ([data]
   (compress data {}))
  ([data opts]
   (-> (if (string? data) (String/.getBytes data "UTF-8") ^byte/1 data)
     (Encoder/compress (encoder-params opts)))))

(defn byte-array-out-stream ^ByteArrayOutputStream []
  (ByteArrayOutputStream/new))

(defn compress-out-stream ^OutputStream
  ([^ByteArrayOutputStream out-stream]
   (compress-out-stream out-stream {}))
  ([^ByteArrayOutputStream out-stream opts]
   (BrotliOutputStream/new out-stream (encoder-params opts) 16384)))

(defn decompress [data]
  (let [decompressed (Decoder/decompress data)]
    (String/new (.getDecompressedData decompressed))))




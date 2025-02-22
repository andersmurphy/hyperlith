(ns hyperlith.impl.gzip
  (:import (java.io ByteArrayOutputStream)
           (java.util.zip GZIPOutputStream)))

(defn byte-array-out-stream ^ByteArrayOutputStream []
  (ByteArrayOutputStream/new))

(defn gzip-out-stream ^GZIPOutputStream [^ByteArrayOutputStream out-stream]
  (GZIPOutputStream/new out-stream true))

(defn gzip
  [data]
  (with-open [out  (byte-array-out-stream)
              gzip (gzip-out-stream out)]
    (doto gzip
      (.write  (if (string? data) (String/.getBytes data) data))
      (.finish))
    (.toByteArray out)))

(defn gzip-chunk [^ByteArrayOutputStream out ^GZIPOutputStream gzip chunk]
  (doto gzip
    (.write  (String/.getBytes chunk))
    (.flush))
  (let [result (.toByteArray out)]
    (.reset out)
    result))

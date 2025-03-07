(ns hyperlith.impl.gzip
  (:require
   [clojure.java.io :as io])
  (:import
   (java.io ByteArrayOutputStream EOFException)
   (java.util.zip GZIPInputStream GZIPOutputStream)))

(defn byte-array-out-stream ^ByteArrayOutputStream []
  (ByteArrayOutputStream/new))

(defn gzip-out-stream ^GZIPOutputStream [^ByteArrayOutputStream out-stream]
  (GZIPOutputStream/new out-stream true))

(defn gzip
  [data]
  (with-open [out  (byte-array-out-stream)
              gzip (gzip-out-stream out)]
    (doto gzip
      (.write  (if (string? data) (String/.getBytes data) ^byte/1 data))
      (.finish))
    (.toByteArray out)))

(defn gzip-chunk [^ByteArrayOutputStream out ^GZIPOutputStream gzip chunk]
  (doto gzip
    (.write  (String/.getBytes chunk))
    (.flush))
  (let [result (.toByteArray out)]
    (.reset out)
    result))

(defn gunzip [data]
  (with-open [in  (-> (if (string? data) (String/.getBytes data) data)
                    io/input-stream
                    GZIPInputStream/new)
              out (ByteArrayOutputStream/new 4096)]
    (try ;; Allows gunzipping of incomplete streams
      (io/copy in out :buffer-size 4096)
      (catch EOFException _))
    (str out)))

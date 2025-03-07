(ns hyperlith.impl.gzip
  (:import
   (java.io ByteArrayInputStream ByteArrayOutputStream EOFException)
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
                    ByteArrayInputStream/new
                    GZIPInputStream/new)
              out (ByteArrayOutputStream/new 4096)]
    (let [buf (byte-array 4096)]
      (try ;; Allows gunzipping of incomplete streams
        (loop [len (.read in buf)]
          (when-not (= len -1)
            (.write out buf 0 len)
            (recur (.read in buf))))
        (catch EOFException _)))
    (str out)))

(ns hyperlith.impl.codec
  (:require [hyperlith.impl.util :as u])
  (:import clojure.lang.MapEntry
           [java.net URLDecoder]
           [java.util StringTokenizer]
           [java.net URLEncoder]))

(defn url-encode [s] (URLEncoder/encode (str s) "UTF-8"))

(defn url-query-string [m]
  (->> (for [[k v] m]
         (str (url-encode (name k)) "=" (url-encode v)))
    (interpose "&")
    (apply str "?")))

(defn form-decode-str
  "Decode the supplied www-form-urlencoded string using UTF-8."
  [^String encoded]
  (try
    (URLDecoder/decode encoded "UTF-8")
    (catch Exception _ nil)))

(defn- tokenized [s delim]
  (reify clojure.lang.IReduceInit
    (reduce [_ f init]
      (let [tokenizer (StringTokenizer. s delim)]
        (loop [result init]
          (if (.hasMoreTokens tokenizer)
            (recur (f result (.nextToken tokenizer)))
            result))))))

(defn- split-key-value-pair [^String s]
  (let [i (.indexOf s #=(int \=))]
    (cond
      (pos? i)  (MapEntry. (.substring s 0 i) (.substring s (inc i)))
      (zero? i) (MapEntry. "" (.substring s (inc i)))
      :else     (MapEntry. s ""))))

(defn form-decode
  "Decode the supplied www-form-urlencoded string using UTF-8. If the encoded
  value is a string, a string is returned. If the encoded value is a map of
  parameters, a map is returned."
  [^String encoded]
  (if-not (.contains encoded "=")
    (form-decode-str encoded)
    (reduce
      (fn [m param]
        (let [kv (split-key-value-pair param)
              k  (form-decode-str (key kv))
              v  (form-decode-str (val kv))]
          (if (and k v)
            (u/assoc-conj m k v)
            m)))
      {}
      (tokenized encoded "&"))))

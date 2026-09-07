(ns hyperlith.impl.html
  (:require
   [dev.onionpancakes.chassis.compiler :as cc]
   [dev.onionpancakes.chassis.core :as h]) 
  (:import
   [java.io BufferedOutputStream ByteArrayOutputStream OutputStream]
   [java.lang Appendable]
   [java.nio.charset StandardCharsets]))

;; Warn on ambiguous attributes
(cc/set-warn-on-ambig-attrs!)

(deftype AppendableOutputStream [^OutputStream out ^java.nio.charset.Charset charset]
  Appendable
  (^Appendable append [this ^CharSequence csq]
   (.write out (.getBytes (.toString csq) charset))
   this)
  (^Appendable append [this ^CharSequence csq ^int start ^int end]
   (.write out (.getBytes (.toString (.subSequence csq start end)) charset))
   this)
  (^Appendable append [this ^char c]
   (.write out (.getBytes (str c) charset))
   this))

(deftype RawBytes [^bytes value]
  h/Token
  (append-fragment-to [_ sb]
    (if (instance? AppendableOutputStream sb)
      (do (.write ^OutputStream (.out ^AppendableOutputStream sb) value)
          sb)
      (do (.append ^Appendable sb
            (String. value StandardCharsets/UTF_8))
          sb)))
  (fragment [_] (String. value StandardCharsets/UTF_8)))

(defn html->stream
  ([out root]
   (html->stream out java.nio.charset.StandardCharsets/UTF_8 root))
  ([^java.io.OutputStream out ^java.nio.charset.Charset charset root]
   (let [buffered (BufferedOutputStream. out 16384)]
     (h/write-html (->AppendableOutputStream buffered charset) root)
     (.flush buffered)
     out)))

(def doctype-html5 h/doctype-html5)

(def html->str h/html)

(def html-raw-str h/raw-string)

(defn html-raw-bytes ^RawBytes [^bytes b]
  (->RawBytes b))

(defmacro html
  "Compiles html."
  [& hiccups]
  (let [node (vec hiccups)]
    `(cc/compile ~node)))

(def html-resolve-alias h/resolve-alias)

(comment
  (html [:div "fooo"])
  (with-open [out (ByteArrayOutputStream/new 4096)]
    (html->stream out
      (html-raw-bytes (String/.getBytes (html->str [:div "fooo"]))))
    (.toByteArray out))
  )

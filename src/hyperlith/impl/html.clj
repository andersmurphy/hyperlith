(ns hyperlith.impl.html
  (:require
   [hyperlith.impl.css :as css]
   [hyperlith.impl.cache :as cache])
  (:import
   [clojure.lang
    APersistentMap
    IPersistentMap
    IPersistentSet
    Keyword
    MapEntry
    Numbers
    Sequential]
   [java.io ByteArrayOutputStream OutputStream]
   [java.lang Iterable]
   [java.nio.charset StandardCharsets]
   [java.util LinkedHashMap HashSet Iterator]))

(set! *warn-on-reflection* true)

(def ^:dynamic ^LinkedHashMap *cache* nil)

(defn write-bytes [^bytes node ^OutputStream out]
  (.write out node))

(defn write-string [^String node ^OutputStream out]
  (.write out
    (String/.getBytes node StandardCharsets/UTF_8)))

(declare write-node)
(declare write-attribute)

(defn str->bytes [s]
  (String/.getBytes s StandardCharsets/UTF_8))

(defn escape ^String [^String value]
  (when value
    (-> value
      (.replace "&" "&amp;")
      (.replace "<" "&lt;")
      (.replace ">" "&gt;")
      (.replace "\"" "&quot;")
      (.replace "'" "&#39;"))))

(let [;; Avoid var lookup overhead (can't have const bytes)
      ^bytes element-open-start-tag          (str->bytes "<")
      ^bytes element-close-start-tag         (str->bytes ">")
      ^bytes element-open-end-tag            (str->bytes "</")
      ^bytes element-close-end-tag           (str->bytes ">")
      ^bytes attribute-separator             (str->bytes " ")
      ^bytes attribute-value-open            (str->bytes "=\"")
      ^bytes attribute-value-close           (str->bytes "\"")
      ^bytes attribute-class-separator       (str->bytes " ")
      ^bytes attribute-declaration-separator (str->bytes ":")
      ^bytes attribute-declaration-end       (str->bytes ";")
      ^HashSet unclosed-tags
      (HashSet. #{"area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "param" "source" "track" "wbr"})]

  (defn write-attribute-boolean
    [^OutputStream out ^String attribute-name]
    (write-bytes attribute-separator out)
    (write-string attribute-name out))

  (defn write-attribute-string
    [^OutputStream out ^String attribute-name ^String attribute-value ^String additional]
    (if (Numbers/isPos (.length attribute-value))
      (do
        (write-bytes attribute-separator out)
        (write-string attribute-name out)
        (write-bytes attribute-value-open out)
        (when additional
          (write-string additional out)
          (write-bytes attribute-class-separator out))
        (write-string attribute-value out)
        (write-bytes attribute-value-close out))
      (when additional
        (write-bytes attribute-separator out)
        (write-string attribute-name out)
        (write-bytes attribute-value-open out)
        (write-string additional out)
        (write-bytes attribute-value-close out))))

  (defn write-attribute-map
    [^OutputStream out ^String attribute-name ^APersistentMap attribute-value]
    (when-not (.isEmpty attribute-value)
      (let [^Iterator iterator (.iterator attribute-value)]
        (write-bytes attribute-separator out)
        (write-string attribute-name out)
        (write-bytes attribute-value-open out)
        (while (.hasNext iterator)
          (let [^MapEntry entry (.next iterator)]
            (write-string (.getName ^Keyword (.key entry)) out)
            (write-bytes attribute-declaration-separator out)
            (write-string (str (.val entry)) out)
            (when (.hasNext iterator)
              (write-bytes attribute-declaration-end out))))
        (write-bytes attribute-value-close out))))

  (defn write-attribute-style
    [^OutputStream out ^String attribute-name ^APersistentMap attribute-value]
    (write-bytes attribute-separator out)
    (write-string attribute-name out)
    (write-bytes attribute-value-open out)
    (write-string ^String (css/style-map->style attribute-value) out)
    (write-bytes attribute-value-close out))

  (defn write-attribute-collection
    [^OutputStream out ^String attribute-name ^Iterable attribute-value ^String additional]
    (let [^Iterator iterator (.iterator attribute-value)]
      (if (.hasNext iterator)
        (do
          (write-bytes attribute-separator out)
          (write-string attribute-name out)
          (write-node attribute-value-open out)
          (when additional
            (write-string additional out)
            (write-bytes attribute-class-separator out))
          (while (.hasNext iterator)
            (when-let [item (.next iterator)]
              (write-string (str item) out)
              (when (.hasNext iterator)
                (write-bytes attribute-class-separator out))))
          (write-bytes attribute-value-close out))
        (when additional
          (write-bytes attribute-separator out)
          (write-string attribute-name out)
          (write-bytes attribute-value-open out)
          (write-string additional out)
          (write-bytes attribute-value-close out)))))

  (defn write-element-attributes
    [^OutputStream out ^APersistentMap attributes]
    (let [^Iterator iterator (.iterator attributes)]
      (while (.hasNext iterator)
        (let [^MapEntry attribute (.next iterator)]
          (when-let [attribute-value (.val attribute)]
            (let [^Keyword attribute-name (.key attribute)]
              (if (or (= attribute-name :id) (= attribute-name :data-id))
                ;; Skip cache on highly variable attributes
                (let [attribute-name-string (.getName attribute-name)]
                  (write-attribute attribute-value
                    attribute-name-string out nil))
                (-> (cache/lookup-or-miss *cache* attribute
                      (fn [_]
                        (with-open [out (ByteArrayOutputStream/new 64)]
                          (let [attribute-name-string (.getName attribute-name)]
                            (write-attribute attribute-value
                              attribute-name-string out nil))
                          (.toByteArray out))))
                  (write-bytes out)))))))))

  (defn write-element-start-tag
    [^OutputStream out ^Iterator element-iterator ^String tag-name]
    (write-bytes element-open-start-tag out)
    (write-string tag-name out)
    (if (.hasNext element-iterator)
      (let [item (.next element-iterator)]
        (if (instance? IPersistentMap item)
          (do
            (write-element-attributes out item)
            (write-bytes element-close-start-tag out))
          (do
            (write-bytes element-close-start-tag out)
            (write-node item out))))
      (write-bytes element-close-start-tag out)))

  (defn write-element-end-tag
    [^OutputStream out ^String element-tag-name]
    (write-bytes element-open-end-tag out)
    (write-string element-tag-name out)
    (write-bytes element-close-end-tag out))

  (defn write-element
    [^OutputStream out ^Iterator element-iterator ^Keyword tag]
    (let [tag-name (.getName tag)]
      (write-element-start-tag out element-iterator tag-name)
      (when-not (.contains unclosed-tags tag-name)
        (while (.hasNext element-iterator)
          (write-node (.next element-iterator) out))
        (write-element-end-tag out tag-name)))))

(defn write-collection
  [^OutputStream out ^Iterable collection]
  (let [^Iterator iterator (.iterator collection)]
    (when (.hasNext iterator)
      (let [item (.next iterator)]
        (if (instance? Keyword item)
          (write-element out iterator item)
          (do (write-node item out)
              (while (.hasNext iterator)
                (write-node (.next iterator) out))))))))

(defn write-node
  [node ^OutputStream out]
  (cond
    (nil? node) nil
    
    (bytes? node) (.write out ^bytes node)
    
    (instance? CharSequence node)
    (.write out (String/.getBytes ^CharSequence node StandardCharsets/UTF_8))
    
    (instance? Sequential node)
    (write-collection out node)
    
    :else (.write out
            (String/.getBytes (str node) StandardCharsets/UTF_8))))

(defn write-attribute [attribute-value attribute-name builder additional]
  (cond
    (instance? String attribute-value)
    (write-attribute-string builder attribute-name attribute-value additional)

    (instance? Boolean attribute-value)
    (write-attribute-boolean builder attribute-name)

    (instance? Sequential attribute-value)
    (write-attribute-collection builder attribute-name attribute-value additional)

    (instance? IPersistentSet attribute-value)
    (write-attribute-collection builder attribute-name attribute-value additional)

    (instance? IPersistentMap attribute-value)
    (if (= attribute-name "style")
      (write-attribute-style builder attribute-name attribute-value)
      (write-attribute-map builder attribute-name attribute-value))

    :else
    (write-attribute-string builder attribute-name (str attribute-value) additional)))

(defn html->stream [out node]
  (write-node node out))

(def doctype-html5 (String/.getBytes "<!DOCTYPE html>"))

(defn html->bytes
  ([node]
   (with-open [out (ByteArrayOutputStream/new 16384)]
     (html->stream out node)
     (.toByteArray out)))
  ([node _throw-away-cache]
   (binding [*cache* (cache/init 3000)]
     (with-open [out (ByteArrayOutputStream/new 16384)]
       (html->stream out node)
       (.toByteArray out)))))

;; Escape string nodes
;; Escape for SSE? (mostly code blocks)?
;; Escape attributes? (fine if we are caching attributes)
;; SIMD/vector escape for those? As we are going to bytes anyway.

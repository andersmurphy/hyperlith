(ns hyperlith.impl.html
  (:require
   [hyperlith.impl.cache :as cache]
   [hyperlith.impl.css :as css]
   [hyperlith.impl.lane-context :as lc])
  (:import
   [clojure.lang
    APersistentMap
    IPersistentMap
    IPersistentSet
    Keyword
    MapEntry
    Numbers
    Sequential]
   [hyperlith.impl.lane_context LaneCtx]
   [java.lang Iterable]
   [java.nio ByteBuffer]
   [java.nio.charset StandardCharsets]
   [java.util HashMap HashSet Iterator]))

(set! *warn-on-reflection* true)

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

(defn buf->array! [^ByteBuffer buf]
  (let [out-array (byte-array (.position buf))]
    (.flip buf)
    (.get buf out-array)
    (.clear buf)
    out-array))

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
      (HashSet. #{"area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "param" "source" "track" "wbr"})
      write-bytes
      (fn write-bytes [^bytes node ^ByteBuffer out]
        (.put out node))
      write-string
      (fn write-string [^String node ^ByteBuffer out]
        (.put out
          (String/.getBytes node StandardCharsets/UTF_8)))]

  (defn write-attribute-string
    [^ByteBuffer out ^String attribute-value]
    (when (Numbers/isPos (.length attribute-value))
      (write-bytes attribute-value-open out)
      (write-string (escape attribute-value) out)
      (write-bytes attribute-value-close out)))

  (defn write-attribute-map
    [^ByteBuffer out ^APersistentMap attribute-value]
    (when-not (.isEmpty attribute-value)
      (let [^Iterator iterator (.iterator attribute-value)]
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
    [^ByteBuffer out ^APersistentMap attribute-value]
    (write-bytes attribute-value-open out)
    (write-string ^String (css/style-map->style attribute-value) out)
    (write-bytes attribute-value-close out))

  (defn write-attribute-collection
    [lane-ctx ^ByteBuffer out ^Iterable attribute-value]
    (let [^Iterator iterator (.iterator attribute-value)]
      (when (.hasNext iterator)
        (write-node lane-ctx attribute-value-open out)
        (while (.hasNext iterator)
          (when-let [item (.next iterator)]
            (write-string (str item) out)
            (when (.hasNext iterator)
              (write-bytes attribute-class-separator out))))
        (write-bytes attribute-value-close out))))

  (defn write-attribute-name [^LaneCtx lane-ctx attribute-name out]
    (let [cache       ^HashMap (.attr-name-cache lane-ctx)
          scratch-out ^ByteBuffer (.attr-byte-scratch lane-ctx)]
      (write-bytes
        ;; Attributes names are finite so we cache them in an unbounded cache
        (or (.get cache attribute-name)
          (let [attribute-string-name (name attribute-name)]
            (write-bytes attribute-separator scratch-out)
            (write-string attribute-string-name scratch-out)
            (let [b (buf->array! scratch-out)]
              (.put cache attribute-name b)
              b)))
        out)))

  (defn write-attribute-pair [^LaneCtx lane-ctx attribute-name attribute-value]
    (let [^ByteBuffer
          scratch-out (.attr-byte-scratch lane-ctx)]
      (write-attribute-name lane-ctx attribute-name
        scratch-out)
      (write-attribute lane-ctx
        attribute-value
        attribute-name scratch-out)
      (buf->array! scratch-out)))

  (defn write-element-attributes 
    [^LaneCtx lane-ctx ^ByteBuffer out ^APersistentMap attributes]
    (let [^Iterator iterator (.iterator attributes)]
      (while (.hasNext iterator)
        (let [^MapEntry attribute (.next iterator)]
          (when-let [attribute-value (.val attribute)]
            (let [^Keyword attribute-name (.key attribute)
                  attr-cache              (.attr-cache lane-ctx)]
              ;; We use caffeine here because we WTinyLRU is much
              ;; better suited to bursts of cache thrashing data
              ;; than a regular LRU.
              ;; 
              ;; Because caffeine cache is being used in a single
              ;; threaded context this is safe and avoids a lot
              ;; of allocations (compared to computeIfAbsent)
              (-> (or (cache/get attr-cache attribute)
                    (cache/put attr-cache attribute
                      (write-attribute-pair lane-ctx attribute-name
                        attribute-value)))
                (write-bytes out))))))))

  (defn write-element-start-tag
    [lane-ctx ^ByteBuffer out ^Iterator element-iterator ^String tag-name]
    (write-bytes element-open-start-tag out)
    (write-string tag-name out)
    (if (.hasNext element-iterator)
      (let [item (.next element-iterator)]
        (if (instance? IPersistentMap item)
          (do
            (write-element-attributes lane-ctx out item)
            (write-bytes element-close-start-tag out))
          (do
            (write-bytes element-close-start-tag out)
            (write-node lane-ctx item out))))
      (write-bytes element-close-start-tag out)))

  (defn write-element-end-tag
    [^ByteBuffer out ^String element-tag-name]
    (write-bytes element-open-end-tag out)
    (write-string element-tag-name out)
    (write-bytes element-close-end-tag out))

  (defn write-element
    [lane-ctx ^ByteBuffer out ^Iterator element-iterator ^Keyword tag]
    (let [tag-name (.getName tag)]
      (write-element-start-tag lane-ctx out element-iterator tag-name)
      (when-not (.contains unclosed-tags tag-name)
        (while (.hasNext element-iterator)
          (write-node lane-ctx (.next element-iterator) out))
        (write-element-end-tag out tag-name)))))

(defn write-collection
  [lane-ctx ^ByteBuffer out ^Iterable collection]
  (let [^Iterator iterator (.iterator collection)]
    (when (.hasNext iterator)
      (let [item (.next iterator)]
        (if (instance? Keyword item)
          (write-element lane-ctx out iterator item)
          (do (write-node lane-ctx item out)
              (while (.hasNext iterator)
                (write-node lane-ctx (.next iterator) out))))))))

(defn write-node
  [lane-ctx node ^ByteBuffer out]
  (cond
    (nil? node) nil

    (bytes? node) (.put out ^bytes node)

    (string? node)
    (.put out (String/.getBytes ^String (escape node)
                StandardCharsets/UTF_8))

    (instance? Sequential node)
    (write-collection lane-ctx out node)

    :else (.put out
            (String/.getBytes
              (escape (str node)) StandardCharsets/UTF_8))))

(defn write-attribute [lane-ctx attribute-value attribute-name builder]
  (cond
    (instance? String attribute-value)
    (write-attribute-string builder attribute-value)

    (instance? Boolean attribute-value) nil

    (instance? Sequential attribute-value)
    (write-attribute-collection lane-ctx builder attribute-value)

    (instance? IPersistentSet attribute-value)
    (write-attribute-collection lane-ctx builder attribute-value)

    (instance? IPersistentMap attribute-value)
    (if (= attribute-name :style)
      (write-attribute-style builder attribute-value)
      (write-attribute-map builder attribute-value))

    :else
    (write-attribute-string builder (str attribute-value))))

(defn html->stream [lane-ctx ^ByteBuffer out node]
  (write-node lane-ctx node out))

(def doctype-html5 (String/.getBytes "<!DOCTYPE html>"))

(defn html->bytes
  ([lane-ctx node]
   (let [out (ByteBuffer/allocate 16384)]
     (html->stream lane-ctx out node)
     (buf->array! out)))
  ([node]
   (let [lane-ctx (lc/map->LaneCtx
                    {:attr-cache        (cache/init 2000)
                     :attr-name-cache   (HashMap.)
                     :attr-byte-scratch (ByteBuffer/allocate 16384)})
         out      (ByteBuffer/allocate 16384)]
     (html->stream lane-ctx out node)
     (buf->array! out))))

(comment
  (html->bytes
    [:div  "hello"
     [:div  "hello"]
     [:div  "hello"]])
  
  (-> (html->bytes
        [:link {:id "css" :rel "stylesheet" :type "text/css" :href
                "/a8jqbDm8qU3PYHTz9IJ9N4k8pKIRzSRSo"}])
    (String.)))

;; Escape for SSE? (mostly code blocks)?
;; Cache open/close tags similar to attr cache

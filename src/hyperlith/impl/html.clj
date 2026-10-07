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
    Sequential]
   [hyperlith.impl.lane_context LaneCtx]
   [java.lang Iterable]
   [java.nio ByteBuffer]
   [java.nio.charset StandardCharsets]
   [java.util HashSet Iterator]))

(set! *warn-on-reflection* true)

(declare write-node)
(declare write-attribute)
(declare html->bytes)

(defn str->bytes [s]
  (String/.getBytes s StandardCharsets/UTF_8))

(defn region->byte-array! [^ByteBuffer out ^long start]
  (let [end (.position out)
        arr (byte-array (- end start))]
    (.position out start)
    (.get out arr)
    (.position out end)
    arr))

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
      (HashSet. #{:area :base :br :col :embed :hr :img :input :link :meta :param :source :track :wbr})

      write-bytes
      (fn write-bytes [^bytes node ^ByteBuffer out]
        (.put out node))

      write-string
      (fn write-string [^String node ^ByteBuffer out]
        (.put out
          (String/.getBytes node StandardCharsets/UTF_8)))

      write-escaped-string
      (fn write-escaped-string [s ^ByteBuffer out]
        (write-string
          (if (string? s)
            (cond-> ^String s
              (not (neg? (.indexOf ^String s "&")))  (.replace "&" "&amp;")
              (not (neg? (.indexOf ^String s "<")))  (.replace "<" "&lt;")
              (not (neg? (.indexOf ^String s ">")))  (.replace ">" "&gt;")
              (not (neg? (.indexOf ^String s "\""))) (.replace "\"" "&quot;")
              (not (neg? (.indexOf ^String s "'")))  (.replace "'" "&#39;"))
            (str s))
          out))

      write-attribute-string
      (fn write-attribute-string
        [^ByteBuffer out attribute-value]
        (when attribute-value
          (write-bytes attribute-value-open out)
          (write-escaped-string attribute-value out)
          (write-bytes attribute-value-close out)))

      write-attribute-map
      (fn write-attribute-map
        [^ByteBuffer out ^APersistentMap attribute-value]
        (when-not (.isEmpty attribute-value)
          (let [^Iterator iterator (.iterator attribute-value)]
            (write-bytes attribute-value-open out)
            (while (.hasNext iterator)
              (let [^MapEntry entry (.next iterator)]
                (write-string (.getName ^Keyword (.key entry)) out)
                (write-bytes attribute-declaration-separator out)
                (write-escaped-string (.val entry) out)
                (when (.hasNext iterator)
                  (write-bytes attribute-declaration-end out))))
            (write-bytes attribute-value-close out))))

      write-attribute-style
      (fn write-attribute-style
        [^LaneCtx lane-ctx ^ByteBuffer out ^APersistentMap attribute-value]
        (write-bytes attribute-value-open out)
        (write-string ^String
          (let [sb (.string-builder lane-ctx)]
            (css/style-map->style sb attribute-value)) out)
        (write-bytes attribute-value-close out))

      write-attribute-collection
      (fn write-attribute-collection
        [lane-ctx ^ByteBuffer out ^Iterable attribute-value]
        (let [^Iterator iterator (.iterator attribute-value)]
          (when (.hasNext iterator)
            (write-node lane-ctx attribute-value-open out)
            (while (.hasNext iterator)
              (when-let [item (.next iterator)]
                (write-escaped-string item out)
                (when (.hasNext iterator)
                  (write-bytes attribute-class-separator out))))
            (write-bytes attribute-value-close out))))

      write-attribute-name
      (fn [^LaneCtx lane-ctx attribute-name ^ByteBuffer out]
        (let [cache (.attr-name-cache lane-ctx)]
          ;; Attributes names are finite so we cache them in an unbounded cache
          ;; Each thread has it's own atom so we can read and then write
          (or (when-let [v (@cache attribute-name)] (write-bytes v out) true)
            (let [start                 (.position out)
                  attribute-string-name (name attribute-name)]
              (write-bytes attribute-separator out)
              (write-string attribute-string-name out)
              (swap! cache assoc attribute-name (region->byte-array! out start))
              nil))))

      write-element-attributes
      (fn write-element-attributes
        [^LaneCtx lane-ctx ^ByteBuffer out ^APersistentMap attributes]
        (reduce-kv
          (fn [_ attr-name attr-value]
            (when attr-value
              (write-attribute-name lane-ctx attr-name out)
              (write-attribute lane-ctx attr-value attr-name out)))
          nil
          attributes))

      write-element-start-tag
      (fn write-element-start-tag
        [^LaneCtx lane-ctx ^ByteBuffer out ^Iterator element-iterator tag]
        (let [start (.position out)
              cache (.tag-open-cache lane-ctx)]
          ;; Tag names are finite so we cache them in an unbounded cache
          ;; Each thread has it's own atom so we can read and then write
          (or (when-let [v (@cache tag)] (write-bytes v out) true)
            (let [tag-name (name tag)]
              (write-bytes element-open-start-tag out)
              (write-string tag-name out)
              (swap! cache assoc tag (region->byte-array! out start))
              nil)))
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

      write-element-end-tag
      (fn write-element-end-tag
        [^LaneCtx lane-ctx ^ByteBuffer out ^Keyword tag]
        (let [start (.position out)
              cache (.tag-close-cache lane-ctx)]
          ;; Tag names are finite so we cache them in an unbounded cache
          ;; Each thread has it's own atom so we can read and then write
          (or (when-let [v (@cache tag)] (write-bytes v out) true)
            (let [tag-name (name tag)]
              (write-bytes element-open-end-tag out)
              (write-string tag-name out)
              (write-bytes element-close-end-tag out)
              (swap! cache assoc tag (region->byte-array! out start))
              nil))))

      write-element
      (fn write-element
        [lane-ctx ^ByteBuffer out ^Iterator element-iterator ^Keyword tag]
        (write-element-start-tag lane-ctx out element-iterator tag)
        (when-not (.contains unclosed-tags tag)
          (while (.hasNext element-iterator)
            (write-node lane-ctx (.next element-iterator) out))
          (write-element-end-tag lane-ctx out tag)))

      write-collection
      (fn write-collection
        [^LaneCtx lane-ctx ^ByteBuffer out ^Iterable collection]
        (let [^Iterator iterator (.iterator collection)]
          (when (.hasNext iterator)
            (let [item (.next iterator)]
              (cond  (keyword? item)
                     (write-element lane-ctx out iterator item)

                     (fn? item)
                     (let [cache (.fragment-cache lane-ctx)
                           b     (or (cache/get cache collection)
                                   (cache/put cache collection
                                     (let [qd (.query-disabled lane-ctx)]
                                         (reset! qd item)
                                       (try
                                         (html->bytes
                                           (apply item
                                             (subvec collection 1))
                                           lane-ctx)
                                         (finally
                                           (reset! qd nil)))
                                       )))]
                       (.put out ^bytes b))

                     :else
                     (do (write-node lane-ctx item out)
                         (while (.hasNext iterator)
                           (write-node lane-ctx (.next iterator) out))))))))]

  (defn write-node
    [lane-ctx node ^ByteBuffer out]
    (cond
      (nil? node) nil

      (bytes? node) (.put out ^bytes node)

      (string? node)
      (write-escaped-string node out)

      (instance? Sequential node)
      (write-collection lane-ctx out node)

      :else (write-escaped-string node out)))
  
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
        (write-attribute-style lane-ctx builder attribute-value)
        (write-attribute-map builder attribute-value))

      :else
      (write-attribute-string builder attribute-value))))

(defn html->stream [node lane-ctx ^ByteBuffer out]
  (write-node lane-ctx node out))

(def doctype-html5 (String/.getBytes "<!DOCTYPE html>"))

(defn html->bytes
  ([node lane-ctx]
   (html->bytes node lane-ctx (ByteBuffer/allocate 16384)))
  ([node lane-ctx ^ByteBuffer out]
   (let [start (.position out)]
     (html->stream node lane-ctx out)
     (let [b (region->byte-array! out start)]
       (.clear out)
       b))))

(defn html->bytes-oneshot
  ([node]
   (html->bytes-oneshot node 16384))
  ([node out-size]
   (let [lane-ctx (lc/new-lane-ctx)
         out      (ByteBuffer/allocate out-size)]
     (html->stream node lane-ctx out)
     (region->byte-array! out 0))))

(comment

  (-> (html->bytes-oneshot
        [:link {:id "css" :rel "stylesheet" :type "text/css" :href
                "/a8jqbDm8qU3PYHTz9IJ9N4k8pKIRzSRSo"}])
    (String.)))

;; Escape for SSE? (mostly code blocks)?
;; Cache open/close tags similar to attr cache

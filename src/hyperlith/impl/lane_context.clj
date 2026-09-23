(ns hyperlith.impl.lane-context 
  (:require
   [hyperlith.impl.cache :as cache]) 
  (:import
   [java.nio ByteBuffer]
   [java.util HashMap]))

(defrecord LaneCtx
    [dbs lane-conns attr-value-cache attr-name-cache byte-scratch
     html-dst-buf zstd-src-buf tag-open-cache tag-close-cache])

(defn new-lane-ctx ^LaneCtx
  ([] (new-lane-ctx nil))
  ([{:keys [dbs lane-conns html-dst-buf]}]
   (map->LaneCtx
     {:dbs              dbs
      :lane-conns       lane-conns
      :tag-open-cache        (HashMap.)
      :tag-close-cache        (HashMap.)
      :attr-value-cache (cache/init 2000)
      :attr-name-cache  (HashMap.)
      ;; Goes back onto the heap when converted to byte array
      :byte-scratch     (ByteBuffer/allocate 16384)
      :html-dst-buf     html-dst-buf})))

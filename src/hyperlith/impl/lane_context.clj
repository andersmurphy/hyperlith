(ns hyperlith.impl.lane-context 
  (:require
   [hyperlith.impl.cache :as cache]))

(defrecord LaneCtx
    [dbs lane-conns attr-value-cache attr-name-cache 
     html-dst-buf zstd-src-buf tag-open-cache tag-close-cache])

(defn new-lane-ctx ^LaneCtx
  ([] (new-lane-ctx nil))
  ([{:keys [dbs lane-conns html-dst-buf]}]
   (map->LaneCtx
     {:dbs              dbs
      :lane-conns       lane-conns
      :tag-open-cache   (atom {})
      :tag-close-cache  (atom {})
      :attr-name-cache  (atom {})
      :attr-value-cache (cache/init 2000)
      :html-dst-buf     html-dst-buf})))

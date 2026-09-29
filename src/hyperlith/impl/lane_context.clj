(ns hyperlith.impl.lane-context)

(defrecord LaneCtx
    [dbs lane-conns attr-name-cache tag-open-cache tag-close-cache
     html-dst-buf fragment-cache])

(defn new-lane-ctx ^LaneCtx
  ([] (new-lane-ctx nil))
  ([{:keys [dbs lane-conns html-dst-buf fragment-cache]}]
   (map->LaneCtx
     {:dbs             dbs
      :lane-conns      lane-conns
      :tag-open-cache  (atom {})
      :tag-close-cache (atom {})
      :attr-name-cache (atom {})
      :html-dst-buf    html-dst-buf
      :fragment-cache  fragment-cache})))

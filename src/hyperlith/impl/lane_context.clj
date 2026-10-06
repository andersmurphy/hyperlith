(ns hyperlith.impl.lane-context)

(defrecord LaneCtx
    [dbs attr-name-cache tag-open-cache tag-close-cache
     html-dst-buf fragment-cache string-builder
     query-disabled])

(defn new-lane-ctx ^LaneCtx
  ([] (new-lane-ctx nil))
  ([{:keys [dbs html-dst-buf fragment-cache query-disabled]}]
   (map->LaneCtx
     {:dbs             dbs
      :tag-open-cache  (atom {})
      :tag-close-cache (atom {})
      :attr-name-cache (atom {})
      :html-dst-buf    html-dst-buf
      :query-disabled  query-disabled
      :string-builder  (StringBuilder. 16384)
      :fragment-cache  fragment-cache})))

(ns hyperlith.impl.lane-context 
  (:import
   [java.util IdentityHashMap]))

(defrecord LaneCtx
    [dbs attr-name-cache tag-open-cache tag-close-cache
     html-dst-buf fragment-cache string-builder
     in-component?])

(defn new-lane-ctx ^LaneCtx
  ([] (new-lane-ctx nil))
  ([{:keys [dbs html-dst-buf fragment-cache in-component?]}]
   (map->LaneCtx
     {:dbs             dbs
      ;; We use IdentityHashMap because keys are keywords
      :tag-open-cache  (IdentityHashMap.)
      :tag-close-cache (IdentityHashMap.)
      :attr-name-cache (IdentityHashMap.)
      :html-dst-buf    html-dst-buf
      :in-component?   in-component?
      :string-builder  (StringBuilder. 16384)
      :fragment-cache  fragment-cache})))

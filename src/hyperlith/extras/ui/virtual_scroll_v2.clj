(ns hyperlith.extras.ui.virtual-scroll-v2
  (:require [hyperlith.core :as h]))

(defn on-intersect-top-js
  [{:keys [handler-path idx a-ref b-ref table-ref
           c-ref translate prev-intersect]}]
  (let [prev-intersect (if (= prev-intersect "jump") "bottom" prev-intersect)]
    (str
      "@post(`"handler-path"?idx="idx"&translate=${"
      (if (= prev-intersect "top")
        (str translate" + $"c-ref".clientHeight")
        (str "$"table-ref".clientHeight - ($"a-ref".clientHeight + $"
          b-ref".clientHeight + "translate")"))
      "}&intersect=top`)")))

(defn on-intersect-bottom-js
  [{:keys [handler-path idx a-ref table-ref b-ref c-ref
           translate prev-intersect]}]
  (let [prev-intersect (if (= prev-intersect "jump") "bottom" prev-intersect)]
    (str
      "@post(`"handler-path"?idx="idx"&translate=${"
      (if (or (= prev-intersect "bottom") (nil? prev-intersect))
        (str translate" + $"a-ref".clientHeight")
        (str "$"table-ref".clientHeight - ($"b-ref".clientHeight + $"
          c-ref".clientHeight + "translate")"))
      "}&intersect=bottom`)")))

(defn on-intersect-jump-js [{:keys [handler-path scroll-ref a-ref
                                    b-ref c-ref item-count a-count]}]
  (str
    ;; Use the average of all three containers to offset
    ;; the translation so that jumps centre the view.
    "@post(`"handler-path"?y=${Math.floor($"scroll-ref
    ".scrollTop - (($"a-ref".clientHeight + $"
    b-ref".clientHeight + $"
    c-ref".clientHeight) /"item-count")*"a-count")}&intersect=jump`)"))

(defn chunk-items [item-fn offset limit]
  (let [items      (vec (item-fn {:offset offset :limit limit}))
        item-count (count items)
        chunk-size (int (/ item-count 3))]
    ;; This is 5-7x faster than partition as there is no iteration
    [(subvec items 0 chunk-size)
     (subvec items chunk-size (* 2 chunk-size))
     (subvec items (* 2 chunk-size) item-count)]))

(defmethod h/html-resolve-alias ::virtual
  [_ {:keys                               [id]
      :v/keys
      [handler-path item-fn item-count-fn min-item-size
       max-rendered-items]
      {:keys [translate idx intersect y]} :v/handler-data
      :as                                 attrs} _]
  (assert (and id handler-path item-fn item-count-fn
            min-item-size max-rendered-items))
  (let [total-item-count (item-count-fn)
        size             (* min-item-size total-item-count)
        scroll-pos       (max (or y 0) 0)
        [idx translate]
        (if (= intersect "jump")
          [(int (* (/ scroll-pos size) total-item-count)) scroll-pos]
          [idx translate])
        offset           (or idx 0)
        limit            max-rendered-items
        max-height       (int (* (/ max-rendered-items 3) min-item-size))
        translate        (max (or translate 0) 0)
        [offset translate intersect]
        (if (> (int (/ limit 3)) offset) [0 0 "bottom"]
            [offset translate intersect])
        [a b c]          (chunk-items item-fn offset limit)
        item-count       (+ (count a) (count b) (count c))
        a-ref            (str "_" id "-v-a-ref")
        b-ref            (str "_" id "-v-b-ref")
        c-ref            (str "_" id "-v-c-ref")
        scroll-ref       (str "_" id "-v-scroll-ref")
        table-ref        (str "_" id "-v-table-ref")
        on-jump          (on-intersect-jump-js
                           {:handler-path handler-path
                            :scroll-ref   scroll-ref
                            :a-ref        a-ref
                            :b-ref        b-ref
                            :c-ref        c-ref
                            ;; a is always full size.
                            ;; c can be less when near end of list.
                            :item-count   item-count
                            :a-count      (count a)})]
    [:div (assoc attrs
            :style {:scroll-behavior     :smooth
                    :overscroll-behavior :contain
                    :overflow-anchor     :none
                    :overflow-y          :auto
                    :overflow-x          :auto
                    :max-height          (str max-height "px")
                    :width               :100%
                    :height              :100%}
            :data-ref scroll-ref)
     [:div {:id       (str id "-v-table")
            :data-ref table-ref
            :style
            {:pointer-events        :none
             :display               :grid
             :height                (str size"px")
             :grid-template-columns 1
             :grid-template-rows
             (if (= intersect "top")
               (str "auto min-content "translate"px")
               (str translate"px min-content auto"))}}
      [:div {;; Ensures new id for each "page"
             :id    (str id "-"idx"-v-top")
             :style {:height :100%}
             :data-on-intersect__once
             (when-not (= offset 0) on-jump)}]
      [:div {:id (str id "-v-abc")}
       ;; This div allows intersect to be placed based on the total size
       ;; of a b c
       [:div {:style {:position :relative :top :20%}
              :data-on-intersect__once
              (when-not (= offset 0)
                (on-intersect-top-js
                  {:handler-path   handler-path
                   :idx            (- offset (count c))
                   :a-ref          a-ref
                   :b-ref          b-ref
                   :c-ref          c-ref
                   :table-ref      table-ref
                   :translate      translate
                   :prev-intersect intersect}))}]
       [:div {:style {:position :relative :top :80%}
              :data-on-intersect__once
              (when-not (>= (+ offset limit) total-item-count)
                (on-intersect-bottom-js
                  {:handler-path   handler-path
                   :idx            (+ offset (count a))
                   :a-ref          a-ref
                   :b-ref          b-ref
                   :c-ref          c-ref
                   :table-ref      table-ref
                   :translate      translate
                   :prev-intersect intersect}))}]
       ;; content hash to make morph more efficient
       [:div {:id (str id "-" (hash a)) :data-ref a-ref} a]
       [:div {:id (str id "-" (hash b)) :data-ref b-ref} b]
       [:div {:id (str id "-" (hash c)) :data-ref c-ref} c]]
      [:div {;; Ensures new id for each "page"
             :id    (str id "-"idx"-v-bottom")
             :style {:height :100%}
             :data-on-intersect__once
             (when-not (>= (+ offset limit) total-item-count) on-jump)}]]]))

;; TODO: add x/y axis headers/sidebar


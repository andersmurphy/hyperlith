(ns hyperlith.extras.ui.virtual-scroll-v2
  (:require [hyperlith.core :as h]))

(defn on-intersect-top-js
  [{:keys [handler-path idx a-ref b-ref table-ref
           c-ref translate prev-intersect]}]
  (str "$_translate = "
    (if (= prev-intersect "top")
      (str translate" + $"c-ref".clientHeight;")
      (str "$"table-ref".clientHeight - ($"a-ref".clientHeight + $"
        b-ref".clientHeight + "translate");"))
    "@post(`"handler-path"?idx="idx
    "&translate=${$_translate}&intersect=top`)"))

(defn on-intersect-bottom-js
  [{:keys [handler-path idx a-ref table-ref b-ref c-ref
           translate prev-intersect]}]
  (str "$_translate = "
    (if (or (= prev-intersect "bottom") (nil? prev-intersect))
      (str translate" + $"a-ref".clientHeight;")
      (str "$"table-ref".clientHeight - ($"b-ref".clientHeight + $"
        c-ref".clientHeight + "translate");"))
    "@post(`"handler-path"?idx="idx
    "&translate=${$_translate}&intersect=bottom`)"))

(defn on-jump-js
  [{:keys [handler-path scroll-ref a-ref top-ref bottom-ref]}]
  (str "$"top-ref".remove();$"bottom-ref".remove();"
    "@post(`"handler-path"?y=${Math.floor($"scroll-ref
    ".scrollTop - $"a-ref".clientHeight)}&intersect=jump`)"))

;; TODO: make it so only one intersect can be triggered in a given frame
;; TODO: then top/bottom refs can be removed

(defmethod h/html-resolve-alias ::virtual
  [_ {:keys                               [id]
      :v/keys                             [handler-path item-fn item-count-fn approx-item-height]
      {:keys [translate idx intersect y]} :v/handler-data
      :as                                 attrs} _]
  (assert (and id handler-path item-fn item-count-fn
            approx-item-height))
  (let [total-item-count (item-count-fn)
        size             (* approx-item-height total-item-count)
        y                (max (or y 0) 0)
        [idx translate intersect]
        (if (= intersect "jump")
          [(int (* (/ y size) total-item-count)) y nil]
          [idx translate intersect])
        offset           (or idx 0)
        limit            60
        translate        (max (or translate 0) 0)
        [offset translate intersect]
        (if (> (int (/ limit 3)) offset) [0 0 nil]
            [offset translate intersect])
        items            (vec (item-fn {:offset offset :limit limit}))
        item-count       (count items)
        chunk-size       (int (/ item-count 3))
        ;; This is 5-7x faster than partition as there is no iteration
        a                (subvec items 0 chunk-size)
        b                (subvec items chunk-size (* 2 chunk-size))
        c                (subvec items (* 2 chunk-size) item-count)
        a-ref            (str "_" id "-virtual-a-ref")
        b-ref            (str "_" id "-virtual-b-ref")
        c-ref            (str "_" id "-virtual-c-ref")
        table-ref        (str "_" id "-virtual-table-ref")
        scroll-ref       (str "_" id "-virtual-scroll-ref")
        top-ref          (str "_" id "-virtual-top-ref")
        bottom-ref       (str "_" id "-virtual-bottom-ref")]
    [:div (assoc attrs
            :style {:scroll-behavior     :smooth
                    :overscroll-behavior :contain
                    :overflow-anchor     :none
                    :overflow-y          :auto
                    :overflow-x          :auto
                    :width               :100%
                    :height              :100%}
            :data-ref scroll-ref)
     [:div {:id       (str id "-virtual-table")
            :style
            {:pointer-events        :none
             :display               :grid
             :height                (str size"px")
             :grid-template-columns 1
             :grid-template-rows
             (if (= intersect "top")
               (str "auto min-content min-content min-content "translate"px")
               (str translate"px min-content min-content min-content auto"))}
            :data-ref table-ref}
      [:div {:id    (str id "-virtual-top")
             :style {:height :100%}
             :data-on-intersect__debounce.100ms
             (when-not (= offset 0)
               (on-jump-js
                 {:handler-path handler-path
                  :scroll-ref   scroll-ref
                  :a-ref        a-ref
                  :top-ref      top-ref
                  :bottom-ref   bottom-ref}))}]
      [:div (assoc {;; content hash to make morph more efficient
                    :id (str id "-" (hash a))}
              :data-ref a-ref)
       [:div {:style    {:position :relative :top :50%}
              :data-ref top-ref
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
       a]
      [:div {;; content hash to make morph more efficient
             :id       (str id "-" (hash b))
             :data-ref b-ref}
       b]
      [:div {;; content hash to make morph more efficient
             :id       (str id "-" (hash c))
             :data-ref c-ref}
       [:div {:style    {:position :relative :top :50%}
              :data-ref bottom-ref
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
       c]
      [:div {:id    (str id "-virtual-bottom")
             :style {:height :100%}
             :data-on-intersect__debounce.100ms
             (when-not (>= (+ offset limit) total-item-count)
               (on-jump-js
                 {:handler-path handler-path
                  :scroll-ref   scroll-ref
                  :a-ref        a-ref
                  :top-ref      top-ref
                  :bottom-ref   bottom-ref}))}]]]))

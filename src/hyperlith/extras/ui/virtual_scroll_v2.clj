(ns hyperlith.extras.ui.virtual-scroll-v2
  (:require [hyperlith.core :as h]))

(defn on-intersect-top-js
  [{:keys [translate-$ post-$ idx a-ref b-ref table-ref
           c-ref translate prev-intersect]}]
  (str
    "$"translate-$" = "
    (if (= prev-intersect "top")
      (str translate" + $"c-ref".clientHeight;")
      (str "$"table-ref".clientHeight - ($"a-ref".clientHeight + $"
        b-ref".clientHeight + "translate");"))
    "$"post-$" = `?idx="idx"&translate=${$"translate-$"}&intersect=top`"))

(defn on-intersect-bottom-js
  [{:keys [translate-$ post-$ idx a-ref table-ref b-ref c-ref
           translate prev-intersect]}]
  (str
    "$"translate-$" = "
    (if (or (= prev-intersect "bottom") (nil? prev-intersect))
      (str translate" + $"a-ref".clientHeight;")
      (str "$"table-ref".clientHeight - ($"b-ref".clientHeight + $"
        c-ref".clientHeight + "translate");"))
    "$"post-$" = `?idx="idx"&translate=${$"translate-$"}&intersect=bottom`"))

(defn on-intersect-jump-js [{:keys [post-$ scroll-ref a-ref]}]
  (str
    "$"post-$" = `?y=${Math.floor($"scroll-ref
    ".scrollTop - $"a-ref".clientHeight)}&intersect=jump`"))

(defn post-effect-js [post-$ handler-path]
  (str
    "if ($"post-$") {"
    "@post(`"handler-path"${$"post-$"}`);"
    "$"post-$" = ''"
    "}"))

(defmethod h/html-resolve-alias ::virtual
  [_ {:keys                               [id]
      :v/keys
      [handler-path item-fn item-count-fn approx-item-height
       max-rendered-items]
      {:keys [translate idx intersect y]} :v/handler-data
      :as                                 attrs} _]
  (assert (and id handler-path item-fn item-count-fn
               approx-item-height max-rendered-items))
  (let [total-item-count (item-count-fn)
        size             (* approx-item-height total-item-count)
        y                (max (or y 0) 0)
        [idx translate intersect]
        (if (= intersect "jump")
          [(int (* (/ y size) total-item-count)) y nil]
          [idx translate intersect])
        offset           (or idx 0)
        limit            max-rendered-items
        translate        (max (or translate 0) 0)
        [offset translate intersect]
        (if (> (int (/ limit 3)) offset) [0 0 "bottom"]
            [offset translate intersect])
        items            (vec (item-fn {:offset offset :limit limit}))
        item-count       (count items)
        chunk-size       (int (/ item-count 3))
        ;; This is 5-7x faster than partition as there is no iteration
        a                (subvec items 0 chunk-size)
        b                (subvec items chunk-size (* 2 chunk-size))
        c                (subvec items (* 2 chunk-size) item-count)
        a-ref            (str "_" id "-virtual-a-sig")
        b-ref            (str "_" id "-virtual-b-sig")
        c-ref            (str "_" id "-virtual-c-sig")
        table-ref        (str "_" id "-virtual-table-sig")
        scroll-ref       (str "_" id "-virtual-scroll-sig")
        post-$           (str "_" id "-virtual-post-sig")
        translate-$      (str "_" id "-virtual-translate-sig")]
    [:div (assoc attrs
            :style {:scroll-behavior     :smooth
                    :overscroll-behavior :contain
                    :overflow-anchor     :none
                    :overflow-y          :auto
                    :overflow-x          :auto
                    :width               :100%
                    :height              :100%}
            :data-ref scroll-ref)
     [:div {:id          (str id "-virtual-table")
            :data-ref    table-ref
            :data-effect (post-effect-js post-$ handler-path)
            :style
            {:pointer-events        :none
             :display               :grid
             :height                (str size"px")
             :grid-template-columns 1
             :grid-template-rows
             (if (= intersect "top")
               (str "auto min-content min-content min-content "translate"px")
               (str translate"px min-content min-content min-content auto"))}}
      [:div {;; Make the idx part of the id to tell morph/datastar that this
             ;; is not the same intersect div even if it's contents and position
             ;; are the same. This ensures it fires when there's a new page.
             :id    (str id "-"idx"-virtual-top")
             :style {:height :100%}
             :data-on-intersect__once__debounce.100ms
             (when-not (= offset 0)
               (on-intersect-jump-js
                 {:post-$     post-$
                  :scroll-ref scroll-ref
                  :a-ref      a-ref}))}]
      [:div (assoc {;; content hash to make morph more efficient
                    :id (str id "-" (hash a))}
              :data-ref a-ref)
       [:div {:style {:position :relative :top :50%}
              :data-on-intersect__once
              (when-not (= offset 0)
                (on-intersect-top-js
                  {:post-$         post-$
                   :translate-$    translate-$
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
       [:div {:style {:position :relative :top :50%}
              :data-on-intersect__once
              (when-not (>= (+ offset limit) total-item-count)
                (on-intersect-bottom-js
                  {:post-$         post-$
                   :translate-$    translate-$
                   :idx            (+ offset (count a))
                   :a-ref          a-ref
                   :b-ref          b-ref
                   :c-ref          c-ref
                   :table-ref      table-ref
                   :translate      translate
                   :prev-intersect intersect}))}]
       c]
      [:div {;; Make the idx part of the id to tell morph/datastar that this
             ;; is not the same intersect div even if it's contents and position
             ;; are the same. This ensures it fires when there's a new page.
             :id    (str id "-"idx"-virtual-bottom")
             :style {:height :100%}
             :data-on-intersect__once__debounce.100ms
             (when-not (>= (+ offset limit) total-item-count)
               (on-intersect-jump-js
                 {:post-$     post-$
                  :scroll-ref scroll-ref
                  :a-ref      a-ref}))}]]]))

;; TODO: add x/y axis headers/sidebar
;; TODO: Read up more on intersection observer API
;; TODO: sometimes double jump. Is debounce better?

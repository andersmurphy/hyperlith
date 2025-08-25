(ns hyperlith.extras.virtual-scroll
  (:require [hyperlith.core :as h]))

(defn resize-js [resize-handler-path]
  (format "@post(`%s?h=${el.clientHeight}&w=${el.clientWidth}`);"
    resize-handler-path))

(defn fetch-next-page-js
  [{:keys [fired-signal bottom top left right scroll-handler-path]}]
  (let [top    (or top 0)
        bottom (or bottom 9007199254740991)
        left   (or left 0)
        right  (or right 9007199254740991)]
    (format
      "if (($%s !== -1) && (%s > el.scrollTop || %s < el.scrollTop || %s > el.scrollLeft || %s < el.scrollLeft))
    {$%s = -1; @post(`%s?x=${Math.floor(el.scrollLeft)}&y=${Math.floor(el.scrollTop)}`);}"
      fired-signal
      top
      bottom
      left
      right
      fired-signal
      scroll-handler-path)))

;; TODO: variable item height?
;; TODO: send up initial device height on connect
;; TODO: auto session/tab state with security/validation

(defn virtual-scroll-logic
  [{:v/keys [item-size max-rendered-items item-count-fn scroll-pos
             view-size item-fn]}]
  (let [scroll-pos         (or scroll-pos 0)
        view-size          (or view-size 1000)
        max-rendered-items (or max-rendered-items 1000)
        visible-items      (min (int (/ view-size item-size))
                             (int (/ max-rendered-items 6)))
        rendered-items     (* 6 visible-items)
        offset-items       (max (- (int (/ scroll-pos item-size))
                                   (- (int (/ rendered-items 2))
                                      (int (/ visible-items 2))))
                             0)
        total-item-count   (item-count-fn)
        remaining-items    (max (- total-item-count offset-items) 0)
        threshold-low      (when (not= offset-items 0)
                             (* (+ offset-items visible-items) item-size))
        threshold-high     (when (> remaining-items rendered-items)
                             (* (- (+ offset-items rendered-items)
                                   visible-items visible-items)
                                item-size))
        max-size           (* (int (/ max-rendered-items 6)) item-size)
        translate          (* offset-items item-size)
        grid-count         (if (> remaining-items rendered-items)
                             rendered-items
                             remaining-items)]

    {:threshold-low    threshold-low
     :threshold-high   threshold-high
     :fired-signal-val offset-items
     :translate        (str translate "px")
     :max-size         (str max-size "px")
     :item-grid-size   (str (* grid-count item-size) "px")
     :item-grid        (str "repeat(" grid-count "," item-size "px)")
     :size             (str (* total-item-count item-size) "px")
     :item-fn          (fn [] (item-fn offset-items rendered-items))}))

(defmethod h/html-resolve-alias ::VirtualX
  [_ {:keys   [id]
      :v/keys [resize-handler-path scroll-handler-path
               _item-size _max-rendered-items _item-count-fn _scroll-pos
               _view-size _item-fn]
      :as     attrs} _]
  (let [{:keys [threshold-low threshold-high item-grid fired-signal-val
                translate max-size size item-fn item-grid-size]}
        (virtual-scroll-logic attrs)
        fired-signal     (str id "fired")
        fetch-next-page? (fetch-next-page-js
                           {:fired-signal        fired-signal
                            :left                threshold-low
                            :right               threshold-high
                            :scroll-handler-path scroll-handler-path})]
    (h/html
      [:div (assoc attrs
              :data-signals (h/edn->json {fired-signal fired-signal-val})
              :data-on-resize__debounce.100ms__window
              (resize-js resize-handler-path)
              :data-on-load   fetch-next-page?
              :data-on-scroll fetch-next-page?
              :style {:scroll-behavior     :smooth
                      :overscroll-behavior :contain
                      :overflow-anchor     :none
                      :overflow-x          :scroll
                      :max-width           max-size
                      :width               :100%})
       [:div
        {:id    (str id "-virtual-table")
         :style {:pointer-events :none
                 :width          size}}
        [:div
         {:id (str id "-virtual-table-view")
          :style
          {;; if width isn't specified explicitly scroll bar will become chaos
           :width                 item-grid-size
           :display               :grid
           :grid-template-columns item-grid
           :transform             (str "translateX(" translate ")")}}
         (item-fn)]]])))

(defmethod h/html-resolve-alias ::VirtualY
  [_ {:keys   [id]
      :v/keys [resize-handler-path scroll-handler-path
               _item-size _max-rendered-items _item-count-fn _scroll-pos
               _view-size _item-fn]
      :as     attrs} _]
  (let [{:keys [threshold-low threshold-high item-grid fired-signal-val
                translate max-size size item-fn item-grid-size]}
        (virtual-scroll-logic attrs)
        fired-signal     (str id "fired")
        fetch-next-page? (fetch-next-page-js
                           {:fired-signal        fired-signal
                            :top                 threshold-low
                            :bottom              threshold-high
                            :scroll-handler-path scroll-handler-path})]
    (h/html
      [:div (assoc attrs
              :data-signals (h/edn->json {fired-signal fired-signal-val})
              :data-on-resize__debounce.100ms__window
              (resize-js resize-handler-path)
              :data-on-load   fetch-next-page?
              :data-on-scroll fetch-next-page?
              :style {:scroll-behavior :smooth
                      :overflow-anchor :none
                      :overflow-y      :scroll
                      :max-height      max-size
                      :height          :100%})
       [:div
        {:id    (str id "-virtual-table")
         :style {:pointer-events :none
                 :height         size}}
        [:div
         {:id (str id "-virtual-table-view")
          :style
          {;; if height isn't specified explicitly scroll bar will become chaos
           :height             item-grid-size
           :display            :grid
           :grid-template-rows item-grid
           :transform          (str "translateY(" translate ")")}}
         (item-fn)]]])))



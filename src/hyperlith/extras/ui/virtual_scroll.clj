(ns hyperlith.extras.ui.virtual-scroll
  (:require [hyperlith.core :as h]
            [clojure.math :as math]))

(defn resize-js [w-signal h-signal resize-handler-path]
  (str "$"w-signal" = el.clientWidth;$"h-signal" = el.clientHeight;"
    "@post('"resize-handler-path"');"))

(defn on-scroll-js [x-signal y-signal]
  (str "$"x-signal"= Math.floor(el.scrollLeft);"
    "$"y-signal"= Math.floor(el.scrollTop);"))

(defn fetch-next-page-js
  [{:keys [x-signal y-signal bottom top left right
           scroll-handler-path]}]
  (let [top    (or top 0)
        bottom (or bottom "Infinity")
        left   (or left 0)
        right  (or right "Infinity")]
    (str "if (("top" > $"y-signal
      " || "bottom" < $"y-signal
      " || "left" > $"x-signal
      " || "right" < $"x-signal")) {" "@post('"scroll-handler-path
      "', {retryMaxCount: Infinity}); el.removeAttribute('data-effect')}")))

(defn virtual-scroll-logic
  [{:keys [item-size max-rendered-items item-count-fn scroll-pos
           view-size buffer-items chunk-size]}]
  (let [chunk-size         (or chunk-size 1)
        scroll-pos         (or scroll-pos 0)
        view-size          (or view-size 1000)
        max-rendered-items (or max-rendered-items 1000)
        buffer-items       (int (or buffer-items
                                  ;; Default to number of items that
                                  ;; fits in 4000px as user scroll speed
                                  ;; not visible items determines how much
                                  ;; you need to buffer
                                  (int (/ 4000 item-size))
                                  ;; TODO: fix if this is larger than max
                                  ;; render
                                  ))
        max-size           (* (int (- max-rendered-items (* 2 buffer-items)))
                             item-size)
        visible-items      (max (int (math/ceil
                                       (/ (min view-size max-size) item-size)))
                             1)
        rendered-items     (+ (* 2 buffer-items) visible-items)
        total-item-count   (item-count-fn)
        offset-items       (max (min (- (math/round (/ scroll-pos item-size))
                                       buffer-items)
                                  ;; can't offset more than total items
                                  (- total-item-count rendered-items))
                             0)
        remaining-items    (- total-item-count offset-items)
        ;; If buffer item is one scroll will be triggered at 50%
        threshold-items    (* 0.5 buffer-items)
        threshold-low      (if (not= offset-items 0)
                             (* (+ offset-items threshold-items) item-size)
                             0)
        threshold-high     (if (> remaining-items rendered-items)
                             (* (- (+ offset-items rendered-items)
                                  visible-items threshold-items)
                               item-size)
                             "Infinity")
        translate          (* offset-items item-size)
        grid-count         (if (> remaining-items rendered-items)
                             rendered-items
                             remaining-items)
        max-size           (str max-size "px")
        item-grid          (str "repeat(" (* chunk-size grid-count) ","
                             (int (/ item-size chunk-size))
                             "px)")
        size               (str (* total-item-count item-size) "px")
        view-size          (str (* chunk-size grid-count
                                  (int (/ item-size chunk-size)))"px")]
    [threshold-low threshold-high offset-items
     max-size item-grid size rendered-items translate view-size]))

(defmethod h/html-resolve-alias ::virtual-table
  [_ {:keys   [id]
      :v/keys [resize-handler-path scroll-handler-path item-fn x y]
      :as     attrs} _]
  (let [[x-threshold-low x-threshold-high x-offset-items
         x-max-size x-item-grid x-size x-rendered-items
         x-translate]
        (when x (virtual-scroll-logic x))
        [y-threshold-low y-threshold-high y-offset-items
         y-max-size y-item-grid y-size y-rendered-items
         y-translate]
        (when y (virtual-scroll-logic y))
        x-signal         (str id "-x")
        y-signal         (str id "-y")
        w-signal         (str id "-w")
        h-signal         (str id "-h")
        fetch-next-page? (fetch-next-page-js
                           {:x-signal            x-signal
                            :y-signal            y-signal
                            :left                x-threshold-low
                            :right               x-threshold-high
                            :top                 y-threshold-low
                            :bottom              y-threshold-high
                            :scroll-handler-path scroll-handler-path})
        {:keys [header sidebar content corner]}
        (item-fn {:x-offset-items   x-offset-items
                  :x-rendered-items x-rendered-items
                  :y-offset-items   y-offset-items
                  :y-rendered-items y-rendered-items})
        x-item-grid      (str "min-content min-content "x-item-grid" auto")
        y-item-grid      (str "min-content min-content "y-item-grid" auto")]
    (h/html
      [:div {:data-signals__ifmissing (h/edn->json {x-signal 0 y-signal 0})
             :style                   {:width      :100%
                                       :height     :100%
                                       :max-width  x-max-size
                                       :max-height y-max-size}}
       [:div
        (assoc attrs
          ;; send up initial size on load
          :data-init
          (resize-js w-signal h-signal resize-handler-path)
          :data-on:resize__debounce.100ms__window
          (resize-js w-signal h-signal resize-handler-path)
          :data-on:scroll (on-scroll-js x-signal y-signal)
          ;; Handles user drag scrolling
          :data-effect fetch-next-page?
          :style {:scroll-behavior     :smooth
                  :overscroll-behavior :contain
                  :overflow-anchor     :none
                  :overflow            :scroll
                  :max-width           x-max-size
                  :max-height          y-max-size
                  :width               :100%
                  :height              :100%})
        [:div {:id    (str id "-virtual-table")
               :style {:position      :relative
                       :width         x-size
                       :height        y-size
                       :display       :grid
                       :grid-template (str y-item-grid "/" x-item-grid)}}
         [:div {:id    (str id "-virtual-corner")
                :style {:display       :grid
                        :grid-template "subgrid/subgrid"
                        :position      :sticky
                        :top           0
                        :left          0
                        :grid-column   1
                        :grid-row      1
                        :z-index       6}}
          corner]
         [:div {:id    (str id "-virtual-header")
                :style {:display       :grid
                        :grid-template "subgrid/subgrid"
                        :position      :sticky
                        :top           0
                        :grid-column   "3/-1"
                        :grid-row      1
                        :z-index       5}}
          header]
         [:div {:id    (str id "-virtual-translate")
                :style {:grid-column 2
                        :grid-row    2
                        :width       (str x-translate "px")
                        :height      (str y-translate "px")}}]
         [:div {:id    (str id "-virtual-sidebar")
                :style {:display       :grid
                        :grid-template "subgrid/subgrid"
                        :position      :sticky
                        :left          0
                        :grid-row      "3/-1"
                        :grid-column   1
                        :z-index       5}}
          sidebar]
         [:div {:id    (str id "-virtual-content")
                :style {:display       :grid
                        :grid-template "subgrid/subgrid"
                        :grid-area     "3/3/-2/-2"}}
          content]]]])))

(defmethod h/html-resolve-alias ::virtual
  [_ {:keys   [id]
      :v/keys [resize-handler-path scroll-handler-path item-fn x y]
      :as     attrs} _]
  (let [[x-threshold-low x-threshold-high x-offset-items
         x-max-size x-item-grid x-size x-rendered-items
         x-translate x-view-size]
        (when x (virtual-scroll-logic x))
        [y-threshold-low y-threshold-high y-offset-items
         y-max-size y-item-grid y-size y-rendered-items
         y-translate y-view-size]
        (when y (virtual-scroll-logic y))
        x-signal         (str id "-x")
        y-signal         (str id "-y")
        w-signal         (str id "-w")
        h-signal         (str id "-h")
        fetch-next-page? (fetch-next-page-js
                           {:x-signal            x-signal
                            :y-signal            y-signal
                            :left                x-threshold-low
                            :right               x-threshold-high
                            :top                 y-threshold-low
                            :bottom              y-threshold-high
                            :scroll-handler-path scroll-handler-path})
        {:keys [content]}
        (item-fn {:x-offset-items   x-offset-items
                  :x-rendered-items x-rendered-items
                  :y-offset-items   y-offset-items
                  :y-rendered-items y-rendered-items})]
    (h/html
      [:div {:data-signals__ifmissing (h/edn->json {x-signal 0 y-signal 0})
             :style                   {:width      :100%
                                       :height     :100%
                                       :max-width  x-max-size
                                       :max-height y-max-size}}
       [:div
        (assoc attrs
          ;; send up initial size on load
          :data-init
          (resize-js w-signal h-signal resize-handler-path)
          :data-on:resize__debounce.100ms__window
          (resize-js w-signal h-signal resize-handler-path)
          :data-on:scroll (on-scroll-js x-signal y-signal)
          ;; Handles user drag scrolling
          :data-effect fetch-next-page?
          :style {:scroll-behavior     :smooth
                  :overscroll-behavior :contain
                  :overflow-anchor     :none
                  :overflow            :scroll
                  :max-width           x-max-size
                  :max-height          y-max-size
                  :width               :100%
                  :height              :100%})
        [:div {:id    (str id "-virtual-table")
               :style {:position                 :relative
                       :width                    x-size
                       :height                   y-size                       
                       :contain                  :strict
                       :pointer-events           :none}}
         ;; Separate translate div to avoid recalculating style of grid
         [:div {:id    (str id "-virtual-translate")
                :style {:position                 :absolute
                        :contain                  :strict
                        :width                    x-view-size
                        :height                   y-view-size
                        :transform
                        (str "translate("x-translate"px,"y-translate"px)")}}
          [:div
           {:id    (str id "-virtual-view")
            :style {:display                  :grid
                    :grid-template
                    (str y-item-grid "/" x-item-grid)
                    :width                    x-view-size
                    :height                   y-view-size
                    :contain                  :strict}}
           content]]]]])))


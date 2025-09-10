(ns hyperlith.extras.ui.virtual-scroll
  (:require [hyperlith.core :as h]
            [clojure.math :as math]))

(defn resize-js [w-signal h-signal resize-handler-path]
  (format "$%s = el.clientWidth; $%s = el.clientHeight; @post('%s');"
    w-signal
    h-signal
    resize-handler-path))

(defn on-scroll-js [x-signal y-signal]
  (format "$%s = Math.floor(el.scrollLeft); $%s = Math.floor(el.scrollTop);"
    x-signal y-signal))

(defn fetch-next-page-js
  [{:keys [x-signal y-signal fired-signal bottom top left right
           scroll-handler-path]}]
  (let [top    (or top 0)
        bottom (or bottom "Infinity")
        left   (or left 0)
        right  (or right "Infinity")]
    (format
      "if (($%s !== -1) && (%s > $%s || %s < $%s || %s > $%s || %s < $%s))
    {$%s = -1; @post('%s', {retryMaxCount: Infinity});}"
      fired-signal
      top
      y-signal
      bottom
      y-signal
      left
      x-signal
      right
      x-signal
      fired-signal
      scroll-handler-path)))

(defn virtual-scroll-logic
  [{:keys [item-size max-rendered-items item-count-fn scroll-pos
           view-size buffer-items]}]
  (let [scroll-pos         (or scroll-pos 0)
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
        ;; If a buffer item is one scroll will be triggered at 50%
        threshold-items    (* 0.5 buffer-items)
        threshold-low      (when (not= offset-items 0)
                             (* (+ offset-items threshold-items) item-size))
        threshold-high     (when (> remaining-items rendered-items)
                             (* (- (+ offset-items rendered-items)
                                  visible-items threshold-items)
                               item-size))
        translate          (* offset-items item-size)
        grid-count         (if (> remaining-items rendered-items)
                             rendered-items
                             remaining-items)
        translate          (str translate "px")
        max-size           (str max-size "px")
        item-grid-size     (str (* grid-count item-size) "px")
        item-grid          (str "repeat(" grid-count "," item-size "px)")
        size               (str (* total-item-count item-size) "px")]
    [threshold-low threshold-high offset-items
     translate max-size item-grid item-grid-size size
     rendered-items]))

(defmethod h/html-resolve-alias ::virtual
  [_ {:keys   [id]
      :v/keys [resize-handler-path scroll-handler-path item-fn x y]
      :as     attrs} content]
  (let [[x-threshold-low x-threshold-high x-offset-items
         x-translate x-max-size x-item-grid x-item-grid-size x-size
         x-rendered-items]
        (when x (virtual-scroll-logic x))
        [y-threshold-low y-threshold-high y-offset-items
         y-translate y-max-size y-item-grid y-item-grid-size y-size
         y-rendered-items]
        (when y (virtual-scroll-logic y))
        x-signal         (str id "-x")
        y-signal         (str id "-y")
        w-signal         (str id "-w")
        h-signal         (str id "-h")
        fired-signal     (str "_" id "fired")
        fetch-next-page? (fetch-next-page-js
                           {:x-signal            x-signal
                            :y-signal            y-signal
                            :fired-signal        fired-signal
                            :left                x-threshold-low
                            :right               x-threshold-high
                            :top                 y-threshold-low
                            :bottom              y-threshold-high
                            :scroll-handler-path scroll-handler-path})]
    (h/html
      [:div {;; make sure signal is initialised before data-on-load
             :data-signals (h/edn->json {fired-signal (str (random-uuid))})
             :data-signals__ifmissing (h/edn->json {x-signal 0 y-signal 0})
             :style        {:width      :100%
                            :height     :100%
                            :max-width  x-max-size
                            :max-height y-max-size}}
       [:div
        (assoc attrs          
          ;; send up initial size on load
          :data-on-load
          (resize-js w-signal h-signal resize-handler-path)
          :data-on-resize__debounce.100ms__window
          (resize-js w-signal h-signal resize-handler-path)
          :data-on-scroll (on-scroll-js x-signal y-signal)
          ;; Handles user drag scrolling
          ;; (if fired, x or y change this runs)
          :data-effect fetch-next-page?
          :style {:scroll-behavior     :smooth
                  :overscroll-behavior :contain
                  :overflow-anchor     :none
                  :overflow-x          (when x :scroll)
                  :overflow-y          (when y :scroll)
                  :max-width           x-max-size
                  :max-height          y-max-size
                  :width               :100%
                  :height              :100%})
        [:div
         {:id    (str id "-virtual-table")
          :style {:pointer-events :none
                  :position       :relative
                  :width          x-size
                  :height         y-size}}
         [:div
          {:id (str id "-virtual-table-view")
           :style
           {:position              :absolute
            ;; if width isn't specified explicitly scroll bar will become chaos
            :width                 x-item-grid-size
            :height                y-item-grid-size
            :display               :grid
            :grid-template-columns x-item-grid
            :grid-template-rows    y-item-grid
            :transform
            (str "translate(" (or x-translate 0) "," (or y-translate 0) ")")}}
          (item-fn {:x-offset-items   x-offset-items
                    :x-rendered-items x-rendered-items
                    :y-offset-items   y-offset-items
                    :y-rendered-items y-rendered-items})]
         ;; Mostly used for things like background SVGs.
         content]]])))


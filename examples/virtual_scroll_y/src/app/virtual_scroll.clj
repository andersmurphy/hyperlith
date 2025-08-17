(ns app.virtual-scroll
  (:require [hyperlith.core :as h]))

(defn fetch-next-page-js [fired-signal bottom top scroll-handler-path]
  (format
    "if (($%s !== -1)  && (%s > el.scrollTop || %s < el.scrollTop))
    {$%s = -1; @post(`%s?y=${Math.floor(el.scrollTop)}`);}"
    fired-signal
    bottom
    top
    fired-signal
    scroll-handler-path))

;; TODO: get user's initial view size
;; TODO: user view resize
;; TODO: variable item height

(defmethod h/html-resolve-alias ::Virtual
  [_
   {:keys   [id]
    :v/keys [row-height max-rendered-rows row-fn row-count-fn
             scroll-handler-path scroll-pos view-height]
    :as     attrs}
   _]
  (let [visible-rows     (min (int (/ view-height row-height))
                           (or max-rendered-rows 1000))
        view-height      (* visible-rows row-height)
        rendered-rows    (* 6 visible-rows)
        shift            (int (- (/ rendered-rows 2) (/ visible-rows 2)))
        scroll-pos       (or scroll-pos 0)
        offset-rows      (max (- (int (/ scroll-pos row-height)) shift) 0)
        total-row-count  (row-count-fn)
        table-height     (* total-row-count row-height)
        threshold        (int (/ rendered-rows 6))
        fired-signal     (str id "fired")
        remaining-rows   (- total-row-count offset-rows)
        fetch-next-page? (fetch-next-page-js fired-signal
                           (if (= offset-rows 0)
                             0
                             (* (+ offset-rows threshold) row-height))
                           (if (> remaining-rows rendered-rows)
                             (* (- (+ offset-rows rendered-rows)
                                   visible-rows threshold)
                                row-height)
                             100000000000)
                           scroll-handler-path)]
    (h/html
      [:div (assoc attrs
              (str "data-signals-" fired-signal)  offset-rows
              :data-on-load   fetch-next-page?
              :data-on-scroll fetch-next-page?
              :style {:scroll-behavior :smooth
                      :overflow-anchor :none
                      :overflow-y      :scroll
                      :height          (str view-height "px")})
       [:div
        {:id    (str id "-virtual-table")
         :style {:pointer-events :none
                 :height         (str table-height "px")}}
        [:div
         {:id (str id "-virtual-table-view")
          :style
          {:position :relative
           :display  :grid
           :grid-template-rows
           (str "repeat(" (if (> remaining-rows rendered-rows)
                            rendered-rows
                            remaining-rows) "," row-height "px)")
           :transform
           (str "translateY(" (* offset-rows row-height) "px)")}}
         (row-fn offset-rows rendered-rows)]]])))

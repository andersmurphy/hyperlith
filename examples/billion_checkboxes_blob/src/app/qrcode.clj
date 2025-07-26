(ns app.qrcode
  (:require [hyperlith.core :as h])
  (:import [io.nayuki.qrcodegen QrCode QrCode$Ecc]))

(defn url->qrcode-svg [url]
  (let [qr   (QrCode/encodeText url QrCode$Ecc/MEDIUM)
        path (let [sb (StringBuilder.)]
               (doseq [y (range 0 (.size qr))
                       x (range 0 (.size qr))]
                 (if (.getModule qr x y)
                   (when (and (not= x 0) (not= y 0)) (.append sb " "))
                   (.append sb (format "M%d,%dh1v1h-1z" x y))))
               (.toString sb))]
    (h/html
      [:svg {:width "100%" :viewBox "0 0 33 33"}
       [:path {:d path}]])))

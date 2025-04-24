(ns hyperlith.impl.html
  (:require [dev.onionpancakes.chassis.core :as h]
            [dev.onionpancakes.chassis.compiler :as cc]))

;; Warn on ambiguous attributes
(cc/set-warn-on-ambig-attrs!)

(def doctype-html5 h/doctype-html5)

(def html->str h/html)

(def html-raw-str h/raw-string)

(defmacro html
  "Compiles html."
  [& hiccups]
  (let [node (vec hiccups)]
    `(cc/compile ~node)))

(def html-resolve-alias h/resolve-alias)

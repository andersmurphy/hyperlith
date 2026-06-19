(ns hyperlith.impl.html
  (:require
   [dev.onionpancakes.chassis.compiler :as cc]
   [dev.onionpancakes.chassis.core :as h]) 
  (:import
   [java.io OutputStream]))

;; Warn on ambiguous attributes
(cc/set-warn-on-ambig-attrs!)

(def doctype-html5 h/doctype-html5)

(defn html->stream
  [^OutputStream out root]
  (let [_ (h/write-html out root)]
    out))

;; TODO: remove
(def html->str h/html)

(def html-raw-str h/raw-string)

(defmacro html
  "Compiles html."
  [& hiccups]
  (let [node (vec hiccups)]
    `(cc/compile ~node)))

(def html-resolve-alias h/resolve-alias)

(defn chassis-data? [x]
  (or (:dev.onionpancakes.chassis.core/content (meta x))
    (= (type x) dev.onionpancakes.chassis.core.RawString)))

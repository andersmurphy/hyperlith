(ns hyperlith.impl.sqlite.ffi-wrapper
  (:require [coffi.ffi :as ffi]
            [clojure.java.io :as io])
  (:import [java.lang.foreign Arena SymbolLookup]))

(defonce ^Arena sqlite-lookup-arena
  (Arena/global))

(def lookup_ (atom nil))

(defn set-library! [file-name]
  (reset! lookup_
    (when file-name
      (SymbolLookup/libraryLookup (.getAbsolutePath (io/file file-name))
        sqlite-lookup-arena))))

 (defn find-symbol [sym]
  (if-let [^SymbolLookup lookup @lookup_]
    (-> lookup (.find (name sym)) (.orElse nil))
    (ffi/find-symbol sym)))

(defmacro defcfn [& args]
  `(with-redefs [coffi.ffi/find-symbol find-symbol]
     (coffi.ffi/defcfn ~@args)))

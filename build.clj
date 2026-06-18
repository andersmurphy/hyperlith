(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'compiling-java)
(def version "0.1.1")
(def class-dir "classes")
(def basis (b/create-basis {:project "deps.edn"}))

(defn clean [_]
  (b/delete {:path "classes"}))

(defn jcompile [_]
  (clean nil)
  (b/javac {:src-dirs   ["src"]
            :class-dir  class-dir
            :basis      basis}))

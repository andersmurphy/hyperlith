(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'app)
(def class-dir "target/classes")
(def basis (delay (b/create-basis {:project "deps.edn"})))
(def uber-file (format "target/%s.jar" (name lib)))

(defn clean [_] (b/delete {:path "target"}))

(defn uber
  [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
  (b/compile-clj {:basis      @basis
                  :ns-compile '[app.main]
                  :src-dirs   ["src"]
                  :class-dir  class-dir
                  :java-opts ;; needed for datalevin
                  ["--add-opens=java.base/java.nio=ALL-UNNAMED"
                   "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"]})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis     @basis
           :main      'app.main}))

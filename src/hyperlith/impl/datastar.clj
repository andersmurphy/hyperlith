(ns hyperlith.impl.datastar
  (:require [hyperlith.impl.assets :refer [static-asset]]
            [hyperlith.impl.json :as j]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def datastar-source-map
  (static-asset
    {:body         (-> "datastar.js.map" io/resource slurp)
     :content-type "text/javascript"
     :gzip?        true}))

(def datastar
  (static-asset
    {:body
     (-> "datastar.js" io/resource slurp
       ;; Make sure we point to the right source map
       (str/replace "datastar.js.map" (:path datastar-source-map)))
     :content-type "text/javascript"
     :gzip?        true}))

(defn merge-fragments [event-id fragments]
  (str "event: datastar-merge-fragments"
    "\nid: " event-id
    "\ndata: fragments " (str/replace fragments "\n" "\ndata: fragments ")
    "\n\n\n"))

(defn merge-signals [signals]
  (str "event: datastar-merge-signals"
    "\ndata: onlyIfMissing false"
    "\ndata: signals " (j/edn->json signals)
    "\n\n\n"))

(def routes
  {[:get (datastar :path)]            (datastar :handler)
   [:get (datastar-source-map :path)] (datastar-source-map :handler)})

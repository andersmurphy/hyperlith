(ns hyperlith.impl.icon
  (:require [hyperlith.impl.assets :refer [static-asset]]
            [clojure.java.io :as io])
  (:import (java.io InputStream)))

(def icon
  (static-asset
    {:body         (-> "icon.png" io/resource io/input-stream
                     InputStream/.readAllBytes)
     :content-type "image/png"}))

(def routes
  {[:get (icon :path)] (icon :handler)})

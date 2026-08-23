(ns hyperlith.impl.blocker
  "Middleware that blocks unwanted traffic."
  (:require [clojure.string :as str]))

(defn wrap-blocker
  [handler]
  (fn [req]
    (cond
      ;; If you don't support zstd you get nothing (bots)
      (not (some-> ((:headers req) "accept-encoding")
             (str/includes? "zstd")))
      {:status 406}

      :else (handler req))))

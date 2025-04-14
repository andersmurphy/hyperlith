(ns hyperlith.impl.blocker
  "Middleware that blocks unwanted traffic.")

(defn wrap-blocker
  [handler]
  (fn [req]
    (cond
      ;; If you don't support Brotli you get nothing (bots).
      (not (->> (get-in req [:headers "accept-encoding"])
             (re-find #"(?:^| )br(?:$|,)")))
      {:status 406}

      :else (handler req))))

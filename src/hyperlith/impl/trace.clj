(ns hyperlith.impl.trace)

(def initial-value {:already-seen #{} :events []})

(def traces_ (atom initial-value))
  
(defn traces-reset!
  "Clear previous logged traces."
  []
  (reset! traces_ initial-value))

(defn traces
  "Logged traces."
  []
  (:events @traces_))

(def trace-tap
  (do (remove-tap trace-tap) ;; Remove old tap
      (let [f (fn [{:keys [exp] :as trace}]
                (swap! traces_
                  (fn [{:keys [already-seen] :as traces}]
                    (if (already-seen (hash exp))
                      traces
                      (-> (update traces :already-seen conj (hash exp))
                        (update :events conj trace))))))]
        (add-tap f)
        f)))

(defmacro trace>
  "Trace macro that logs expression and return value to traces atom via
  tap>. Only logs a given expression once."
  [args]
  `(let [result# ~args]
     (tap> (sorted-map :exp (quote ~args) :ret result# :meta ~(meta args)))
     result#))

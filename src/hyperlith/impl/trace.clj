(ns hyperlith.impl.trace)

(def traces_ (atom []))

(defn traces [] @traces_)

(defn reset-traces! []
  (reset! traces_ []))

(def trace-tap
  (do (remove-tap trace-tap) ;; Remove old tap
      (let [f (partial swap! traces_ conj)]
        (add-tap f)
        f)))

(defmacro trace>
  "Trace macro that logs expression and return value to traces atom via
  tap>."
  [args]
  `(let [result# ~args]
     (tap> (sorted-map :exp (quote ~args) :ret result#))
     result#))


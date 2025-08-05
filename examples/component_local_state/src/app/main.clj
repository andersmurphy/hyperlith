(ns app.main
  (:gen-class)
  (:require [hyperlith.core :as h :refer [defview]]))

(def css
  (h/static-css
    [["*, *::before, *::after"
      {:box-sizing :border-box
       :margin     0
       :padding    0}]

     [:html
      {:font-family "Arial, Helvetica, sans-serif"}]

     [:.main
      {:height        :100dvh
       :width         "min(100% - 2rem , 40rem)"
       :margin-inline :auto
       :padding-block :2dvh
       :display       :grid
       :place-items   :center}]

     [:.counter
      {:text-align :center
       :font-size  :50px}]

     [:.button
      {:background    :white
       :text-align    :center
       :font-size     :1.2rem
       :border-radius :0.15em
       :border        "0.15em solid currentColor"
       :padding       :5px}]

     [:.accordion
      {:background    :white
       :text-align    :center
       :font-size     :1.2rem
       :border-radius :0.15em
       :border        "0.15em solid currentColor"
       :padding       :5px}]

     ["@keyframes pop"
      "{  0% {transform: scale(1);}
           25% {transform: scale(0.8);}
          100% {transform: scale(1);}}"]

     [:.pop
      {;; Animation that depresses the element
       :animation      "pop .3s ease"
       ;; Disable element until this class is removed
       :pointer-events :none}]]))

(def shim-headers
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]))

(defmethod h/html-resolve-alias ::Accordion
  [_ {:keys [id] :as _attrs} content]
  (let [open-signal (str "_" id "-accordion-open")]
    (h/html
      [:div.accordion
       {:data-signals__if_missing {open-signal false}}
       [:div.button
        {:data-on-mousedown (str "$" open-signal "= !$" open-signal)
         :data-text         (str "$" open-signal " ? 'Close' : 'Open'")}]
       [:div.content {:data-show (str "$" open-signal)}
        content]])))

(defview render-home
  {:path         "/"
   :shim-headers shim-headers}
  [{:keys [_db] :as _req}]
  (h/html
    [:link#css {:rel "stylesheet" :type "text/css" :href css}]
    [:main#morph.main
     [::Accordion#ac1
      [::Accordion#ac2
       [::Accordion#ac3 [:p "hello world"]]]]]))

(defn ctx-start []{})

(defn -main [& _]
  (h/start-app
    {:max-refresh-ms 100
     :ctx-start       ctx-start
     :ctx-stop        (fn [_state] nil)
     :csrf-secret    (h/env :csrf-secret)}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (def app (-main))
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  ((app :stop))

  ,)

(ns app.main
  (:gen-class)
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [hyperlith.core :as h]))

(def css
  (h/static-css
    [["*, *::before, *::after"
      {:box-sizing :border-box
       :margin     0
       :padding    0}]

     [:.main
      {:height        :100dvh
       :width         "min(100% - 2rem , 40rem)"
       :margin-inline :auto
       :display       :grid
       :place-items   :center}]

     [:.form
      {:display        :flex
       :flex-direction :column
       :gap            :5px}]

     [:.field
      {:display        :flex
       :flex-direction :column
       :gap            :5px}]]))

(def nbsp "\u00A0")

(defmacro validated-field [signal validation-endpoint]
  (let [signal-str (str (symbol signal))]
    `(h/html
       [:div.field
        [:h3 ~(str (str/capitalize signal-str) ":")]
        [:input {:type "text" :data-bind ~signal-str :value ~signal
                 :data-on-keydown__debounce.300ms
                 ~(str "@post('" validation-endpoint "')")}]
        [:label (array-map ;; order matters
                  (keyword ~(str "data-signals-error-" signal-str
                              "__case.kebab__ifmissing"))
                  ~(str "'" nbsp "'")
                  :data-text ~(str "$error-" signal-str))]])))

(defn render-home [{:keys [db sid] :as _req}]
  (let [{:keys [name email company twitter]} (@db sid)]
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:main#morph.main
       (if (get-in @db [sid :submitted])
         (h/html
           [:h1 "Form submitted successfully"])
         (h/html 
           [:div.form
            [:h1 "Form"]
            (validated-field name    "/validate")
            (validated-field email   "/validate")
            (validated-field company "/validate")
            (validated-field twitter "/validate")
            [:button
             {:data-on-click "@post('/submit')"} "Submit"]]))])))

(defn action-validate-fields
  [{:keys                                [_sid]
    {:keys [name email company twitter]} :body}]
  (h/signals
    {:error-name    (if (str/blank? name) "Need a name" nbsp)
     :error-email   (if (str/blank? email) "Need an email" nbsp)
     :error-company (if (str/blank? company) "Need a company" nbsp)
     :error-twitter (if (str/blank? twitter) "Need a twitter" nbsp)}))

(defn action-submit-fields
  [{:keys [sid db body]}]
  (let [fields (select-keys body [:name :email :company :twitter])]
    (when-not (some str/blank? (vals fields))
      (swap! db assoc sid (assoc fields :submitted "true")))))

(def default-shim-handler
  (h/shim-handler
    (h/html
      [:link#css {:rel "stylesheet" :type "text/css" :href (css :path)}]
      [:title nil "Form"]
      [:meta {:content "Form app" :name "description"}])))

(def router
  (h/router
    {[:get (css :path)]  (css :handler)
     [:get  "/"]         default-shim-handler
     [:post "/"]         (h/render-handler #'render-home)
     [:post "/validate"] (h/action-handler #'action-validate-fields)
     [:post "/submit"]   (h/action-handler #'action-submit-fields)}))

(defn ctx-start []
  (let [db_ (atom {})]
    (add-watch db_ :refresh-on-change (fn [& _] (h/refresh-all!)))
    {:db db_}))

(defn -main [& _]
  (h/start-app
    {:router         #'router
     :max-refresh-ms 100
     :ctx-start      ctx-start
     :ctx-stop       (fn [_state] nil)
     :csrf-secret    (h/env :csrf-secret)
     :on-error       (fn [_ctx {:keys [req error]}]
                       (pprint/pprint req)
                       (pprint/pprint error))}))

;; Refresh app when you re-eval file
(h/refresh-all!)

(comment
  (-main)
  ;; (clojure.java.browse/browse-url "http://localhost:8080/")

  ;; stop server
  (((h/get-app) :stop))

  (def db (-> (h/get-app) :ctx :db))
  (reset! db {})
  ,)

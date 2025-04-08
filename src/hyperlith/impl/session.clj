(ns hyperlith.impl.session
  (:require [hyperlith.impl.crypto :as crypto]))

(defn get-sid [req]
  (try ;; In case we get garbage
    (some->> (get-in req [:headers "cookie"])
      (re-find #"__Host-sid=([^;^ ]+)")
      second)
    (catch Throwable _)))

(defn session-cookie [sid]
  (str "__Host-sid=" sid "; Path=/; Secure; HttpOnly; SameSite=Lax"))

(defn csrf-cookie [csrf]
  (str "__Host-csrf=" csrf "; Path=/; Secure; SameSite=Lax"))

(defn wrap-session
  [handler csrf-secret]
  (let [;; Only create the spec once.
        csrf-keyspec (crypto/secret-key->hmac-md5-keyspec csrf-secret)
        sid->csrf    (fn sid->csrf [sid] (crypto/hmac-md5 csrf-keyspec sid))]
    (fn [req]
      (let [body (:body req)
            sid  (get-sid req)]
        (cond
          ;; If user has sid and csrf handle request
          (and sid (= (:csrf body) (sid->csrf sid)))
          (handler (assoc req :sid sid :csrf (:csrf body)))

          ;; :get request and user does not have session we create one
          ;; if they do not have a csrf cookie we give them one
          (= (:request-method req) :get)
          (let [new-sid (or sid (crypto/random-unguessable-uid))
                csrf    (sid->csrf new-sid)]
            (-> (handler (assoc req :sid new-sid :csrf csrf))
              (assoc-in [:headers "Set-Cookie"]
                ;; These cookies won't be set on local host on chrome/safari
                ;; as it's using secure needs to be true and local host
                ;; does not have HTTPS. SameSite is set to lax as it
                ;; allows the same cookie session to be used following a
                ;; link from another site.
                [(session-cookie new-sid)
                 (csrf-cookie csrf)])))

          ;; Not a :get request and user does not have session we 403
          ;; Note: If the updates SSE connection is a not a :get then this
          ;; will close the connection until the user reloads the page.
          :else
          {:status 403})))))

(def csrf-cookie-js
  "document.cookie.match(/(^| )__Host-csrf=([^;]+)/)?.[2]")

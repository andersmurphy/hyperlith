(ns hyperlith.impl.session
  (:require [hyperlith.impl.crypto :as crypto]))

(defn get-sid [req]
  (some->> (get-in req [:headers "cookie"])
    (re-find #"__Host-sid=([^;^ ]+)")
    second))

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
          ;; and a csrf cookie
          (= (:request-method req) :get)
          (let [new-sid (crypto/random-unguessable-uid)
                csrf    (sid->csrf new-sid)]
            (-> (handler (assoc req :sid new-sid :csrf csrf))
              (assoc-in [:headers "Set-Cookie"]
                ;; This cookie won't be set on local host on chrome/safari as
                ;; it's using secure needs to be true and local host
                ;; does not have HTTPS. SameSite is set to lax as it
                ;; allows the same cookie session to be used following a
                ;; link from another site.
                [(str ;; Set session cookie
                   "__Host-sid=" new-sid
                   "; Path=/; Secure; HttpOnly; SameSite=Lax")
                 (str ;; Set csrf cookie
                   "__Host-csrf=" csrf
                   "; Path=/; Secure; SameSite=Lax")])))

          ;; not a :get request and user does not have session we 403
          :else
          {:status 403})))))

(def csrf-cookie-js
  "document.cookie.match(/(^| )__Host-csrf=([^;]+)/)?.[2]")

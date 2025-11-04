(ns hyperlith.impl.crypto
  (:import [java.security SecureRandom]
           [java.security MessageDigest]
           [java.util Base64 Base64$Encoder]))

(def ^SecureRandom secure-random
  (SecureRandom/new))

(def ^Base64$Encoder base64-encoder
  (.withoutPadding (Base64/getUrlEncoder)))

(defn bytes->base64 [^byte/1 b]
  (.encodeToString base64-encoder b))

(defn random-unguessable-uid
  "URL-safe base64-encoded 160-bit (20 byte) random value. Speed
  is similar random-uuid.
  See: https://neilmadden.blog/2018/08/30/moving-away-from-uuids/"
  []
  (let [buffer (byte-array 20)]
    (.nextBytes secure-random buffer)
    (bytes->base64 buffer)))

(def new-uid
  "Allows uid implementation to be changed if need be."
  random-unguessable-uid)

(defn digest
  "Short digest, compact but with a higher collision rate."
  [data]
  (let [^byte/1 bytes (if (bytes? data)
                        data
                        (String/.getBytes (str data)))]
    (-> (doto (MessageDigest/getInstance "SHA256")
          (MessageDigest/.update bytes))
      (MessageDigest/.digest)
      bytes->base64
      (subs 10))))

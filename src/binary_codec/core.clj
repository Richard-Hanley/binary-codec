(ns binary-codec.core
  (:require [clojure.spec.alpha :as s]))

(defprotocol Codec
  (alignment* [this word-size])
  (put-buffer* [this data buffer])
  (get-buffer* [this buffer])
  (sizeof* [this]))

(defn codec?
  "returns c if c is a codec, else false"
  [c]
  (and (extends? Codec (class c)) c))
  

(defonce ^:private registry-ref (atom {}))

(defn registry
    "returns the registry map, prefer 'get-codec' to lookup a codec by name"
    []
    @registry-ref)

(defn- with-name [spec name]
    (with-meta spec (assoc (meta spec) ::name name)))

(defn- named? [x] (instance? clojure.lang.Named x))

(defn- reg-resolve
    "returns the codec end of alias chain starting with k, nil if not found, k if k not Named"
    [k]
    (if (named? k)
      (let [reg @registry-ref]
        (loop [spec k]
          (if (named? spec)
            (recur (get reg spec))
            (when spec
              (with-name spec k)))))
      k))

(defn def-impl [k codec spec]
  ; (swap! registry-ref assoc k codec)
  (println k)
  (macroexpand '(s/def k spec)))
  ; k)

(defmacro def [k [codec spec]]
  `(do
     (swap! registry-ref assoc ~k ~codec)
     (if (not= ~k ~spec)
       (s/def ~k ~spec))
     ~k))

(s/def ::foo even?)

(binary-codec.core/def ::int8 
  [(reify Codec
    (alignment* [_ _] Byte/BYTES)
    (put-buffer* [_ data buffer] (.put buffer data))
    (get-buffer* [_ buffer] (.get buffer))
    (sizeof* [_] Byte/BYTES))
   (s/conformer #(try (byte %) (catch IllegalArgumentException e ::s/invalid)))])

; (binary-codec.core/def ::int16 
;   (reify Codec
;     (alignment* [_ _] Short/BYTES)
;     (put-buffer* [_ data buffer] (.putShort buffer data))
;     (get-buffer* [_ buffer] (.getShort buffer))
;     (sizeof* [_] Short/BYTES)))

; (binary-codec.core/def ::int32
;   (reify Codec
;     (alignment* [_ _] Integer/BYTES)
;     (put-buffer* [_ data buffer] (.putInt buffer data))
;     (get-buffer* [_ buffer] (.getInt buffer))
;     (sizeof* [_] Integer/BYTES)))

; (binary-codec.core/def ::int64
;   (reify Codec
;     (alignment* [_ _] Long/BYTES)
;     (put-buffer* [_ data buffer] (.putLong buffer data))
;     (get-buffer* [_ buffer] (.getLong buffer))
;     (sizeof* [_] Long/BYTES)))



(defn alignement [codec-or-k word-size]
  (let [codec (reg-resolve codec-or-k)]
    (if (codec? codec)
      (alignment* codec word-size))))

(defn put-buffer [codec-or-k data buffer]
  (let [codec (reg-resolve codec-or-k)]
    (if (codec? codec)
      (put-buffer* codec data buffer))))
  
(defn get-buffer [codec-or-k buffer]
  (let [codec (reg-resolve codec-or-k)]
    (if (codec? codec)
      (get-buffer* codec buffer))))

(defn sizeof [codec-or-k]
  (let [codec (reg-resolve codec-or-k)]
    (if (codec? codec)
      (sizeof* codec))))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

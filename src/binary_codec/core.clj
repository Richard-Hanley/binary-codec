(ns binary-codec.core
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [clojure.spec.alpha :as s]))

(alias 'c 'clojure.core)

(defprotocol Codec
  (alignment*     [this encoding] "The alignment of this codec with a given encoding")
  (sizeof*        [this encoding data] "Given a piece of data, and an encoding, 
                                       what is the number of bytes that it will fill")
  (to-buffer!*    [this encoding data buffer] "Serialize data to a passed in buffer, based on the
                                              associated encoding")
  (from-buffer!*  [this encoding buffer] "Deserialize from a binary buffer to data, based on the
                                         associated encoding"))

(defn alignment-padding [align-to position]
  (if-not (zero? align-to)
    (let [offset (mod position align-to)]
      (if-not (zero? offset)
        (- align-to offset)
        0))
    0))

(defn- align-buffer-write [align-to buffer]
  (let [position (.position buffer)
        padding (alignment-padding align-to position)]
    (dotimes [n padding]
      (.put buffer (byte 0)))))

(defn- align-buffer-read [align-to buffer]
  (let [position (.position buffer)
        padding (alignment-padding align-to position)]
    (dotimes [n padding] 
      (.get buffer))))

(defn- make-signed-integral-conformer [checked-converter]
  (s/conformer (fn [value]
                 (try 
                   (checked-converter value)
                   (catch Exception e ::s/invalid)))))

(defn- make-unsigned-integral-conformer [bit-length checked-converter unchecked-converter]
  (let [shift-value (- Long/SIZE bit-length)
        sized-bitmask (unsigned-bit-shift-right -1 shift-value)]
    (s/conformer 
      (fn [value]

        (try 
          (checked-converter value)
          (catch IllegalArgumentException e 
            (if (= (bit-and value sized-bitmask) value)
              (unchecked-converter value)
              ::s/invalid))
          (catch Exception e ::s/invalid))))))

(defn codec?
  "returns c if c is a codec, else false"
  [c]
  (and (satisfies? Codec c) c))


(defonce ^:private registry-ref (atom {}))

(defn registry
  "returns the registry map, prefer 'get-codec' to lookup a codec by name"
  []
  @registry-ref)

(defn- with-name [spec name]
  (with-meta spec (assoc (meta spec) ::name name)))

(defn- named? [x] (instance? clojure.lang.Named x))

(defn reg-resolve
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


(defn def 
  "Macro used to define a global codec, a la spec.  It takes a fully qualified keyword
  as and a codec, and adds that codec to the registry"
  [k codec] 
  (do
    (swap! registry-ref assoc k codec)
    k))

(s/def ::word-size #{0 1 2 4 8})

(s/def ::base-encoding (s/keys :req [::word-size]))

(def base-encoding {::word-size 0})

(defn alignment
  ([codec-or-k] (alignment codec-or-k base-encoding))
  ([codec-or-k encoding] 
   (if-let [codec (reg-resolve codec-or-k)]
     (alignment* codec encoding)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn sizeof 
  ([codec-or-k] (sizeof codec-or-k base-encoding nil))
  ([codec-or-k encoding] (sizeof codec-or-k encoding nil))
  ([codec-or-k encoding data]
   (if-let [codec (reg-resolve codec-or-k)]
     (sizeof* codec encoding data)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn to-buffer!
  ([codec-or-k data buffer] (to-buffer! codec-or-k base-encoding data buffer))
  ([codec-or-k encoding data buffer]
   (if-let [codec (reg-resolve codec-or-k)]
     (do
       (align-buffer-write (alignment* codec encoding) buffer)
       (to-buffer!* codec encoding data buffer))
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn from-buffer!
  ([codec-or-k buffer] (from-buffer! codec-or-k base-encoding buffer))
  ([codec-or-k encoding buffer]
   (if-let [codec (reg-resolve codec-or-k)]
     (do
       (align-buffer-read (alignment* codec encoding) buffer)
       (from-buffer!* codec encoding buffer))
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(binary-codec.core/def ::int8 
  (reify Codec
    (alignment* [_ _] 0)
    (sizeof* [_ _ _] Byte/BYTES)
    (to-buffer!* [_ _ data buffer] (.put buffer data))
    (from-buffer!* [_ _ buffer] (.get buffer))))
(s/def ::int8 (make-signed-integral-conformer byte))

(binary-codec.core/def ::int16
  (reify Codec
    (alignment* [_ encoding] 
      (let [{word-size ::word-size} (s/conform ::base-encoding encoding)]
        (min word-size Short/BYTES)))
    (sizeof* [_ _ _] Short/BYTES)
    (to-buffer!* [_ _ data buffer] (.putShort buffer data))
    (from-buffer!* [_ _ buffer] (.getShort buffer))))
(s/def ::int16 (make-signed-integral-conformer short))

(binary-codec.core/def ::int32
  (reify Codec
    (alignment* [_ encoding]
      (let [{word-size ::word-size} (s/conform ::base-encoding encoding)]
        (min word-size Integer/BYTES)))
    (sizeof* [_ _ _] Integer/BYTES)
    (to-buffer!* [_ _ data buffer] (.putInt buffer data))
    (from-buffer!* [_ _ buffer] (.getInt buffer))))
(s/def ::int32 (make-signed-integral-conformer int))

(binary-codec.core/def ::int64
  (reify Codec
    (alignment* [_ encoding]
      (let [{word-size ::word-size} (s/conform ::base-encoding encoding)]
        (min word-size Long/BYTES)))
    (sizeof* [_ _ _] Long/BYTES)
    (to-buffer!* [_ _ data buffer] (.putLong buffer data))
    (from-buffer!* [_ _ buffer] (.getLong buffer))))
(s/def ::int64 (make-signed-integral-conformer long))

(binary-codec.core/def ::uint8 ::int8)
(s/def ::uint8 (make-unsigned-integral-conformer Byte/SIZE byte unchecked-byte))

(binary-codec.core/def ::uint16 ::int16)
(s/def ::uint16 (make-unsigned-integral-conformer Short/SIZE short unchecked-short))

(binary-codec.core/def ::uint32 ::int32)
(s/def ::uint32 (make-unsigned-integral-conformer Integer/SIZE int unchecked-int))

(defn lazy-pad
  "Returns a lazy sequence which pads sequence with pad-value."
  [sequence pad-value]
  (if (empty? sequence)
    (repeat pad-value)
    (lazy-seq (cons (first sequence) (lazy-pad (rest sequence) pad-value)))))

(extend-protocol Codec

  clojure.lang.Sequential
  (alignment* [this encoding] (apply max (map #(alignment % encoding) this)))
  (sizeof* [this encoding data]
    (reduce 
      (fn [accum [codec elem]]
        (if-let [size (sizeof codec encoding elem)]
          (+ accum size (alignment-padding (alignment codec encoding) accum))
          (reduced nil)))
      0
      (map vector this (lazy-pad data nil))))
  (to-buffer!* [this encoding data buffer] 
    (doseq [[codec elem] (map vector this data)]
      (to-buffer! codec encoding elem buffer))
    buffer)
  (from-buffer!* [this encoding buffer]
    (into [] (doall (map #(from-buffer! % encoding buffer) this))))

  clojure.lang.PersistentArrayMap
  (alignment* [this encoding] 
    (alignment (vals this) encoding))
  (sizeof* [this encoding data] 
    (sizeof (vals this) encoding (vals data)))
  (to-buffer!* [this encoding data buffer] 
    (to-buffer! (vals this) encoding (map #(get data %1) (keys this)) buffer))
  (from-buffer!* [this encoding buffer] 
    (let [values (from-buffer! (vals this) encoding buffer)]
      (into {} (map vector (keys this) values)))))

(defn align [alignment-value codec]
  "Creates a wrapper around the passed codec that forces the word-size to be the given value"
  (reify Codec
    (alignment*[_ encoding] (alignment codec (assoc encoding ::word-size alignment-value)))
    (sizeof* [_ encoding data] (sizeof* codec (assoc encoding ::word-size alignment-value) data))
    (to-buffer!* [_ encoding data buffer] (to-buffer!* codec (assoc encoding ::word-size alignment-value) data buffer))
    (from-buffer!* [_ encoding buffer] (from-buffer!* codec (assoc encoding ::word-size alignment-value) buffer))))

(defn unaligned [codec]
  "Creates a wrapper around the passed codec that forces the word-size to be 0"
  (reify Codec
    (alignment*[_ encoding] (alignment codec (assoc encoding ::word-size 0)))
    (sizeof* [_ encoding data] (sizeof codec (assoc encoding ::word-size 0) data))
    (to-buffer!* [_ encoding data buffer] (to-buffer! codec (assoc encoding ::word-size 0) data buffer))
    (from-buffer!* [_ encoding buffer] (from-buffer! codec (assoc encoding ::word-size 0) buffer))))

(def byte-orders {:endian/little ByteOrder/LITTLE_ENDIAN
                  :endian/big ByteOrder/BIG_ENDIAN
                  :endian/network ByteOrder/BIG_ENDIAN
                  :endian/native (ByteOrder/nativeOrder)})

(defn force-byte-order [order codec]
  "Creates a wrapper for the passed codec that forces byte buffers to be in specific byte order.
  The order can either be a java.nio.ByteOrder object, or it could be a keyword from the byte-order
  map.  Supported keywords are :endian/little :endian/big :endian/network :endian/native"
  (let [byteorder (if (keyword? order)
                    (order byte-orders)
                    order)]
    (reify Codec
      (alignment*[_ encoding] (alignment codec encoding))
      (sizeof* [_ encoding data] (sizeof codec encoding data))
      (to-buffer!* [_ encoding data buffer] 
        (let [prev-order (.order buffer)]
          (do
            (.order buffer byteorder)
            (to-buffer! codec encoding data buffer)
            (.order buffer prev-order))))
      (from-buffer!* [_ encoding buffer]
        (let [prev-order (.order buffer)
              value (from-buffer! codec encoding (.order buffer byteorder))]
          (do
            (.order buffer prev-order)
            value))))))



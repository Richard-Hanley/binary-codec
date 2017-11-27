(ns binary-codec.core
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [binary-codec.encoding :as encoding]
            [clojure.spec.alpha :as s]))

(defprotocol Codec
  (encode*        [this encoding] "Encodes a codec with the given parameters, and returns a new codec")
  (encoded?*       [this]          "Returns a boolean of whether or not the codec is fully encoded, or if it
                                  needs more encoding parameters")
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
  "Function used to define a global codec, a la spec.  It takes a fully qualified keyword
  as and a codec, and adds that codec to the registry"
  [k codec] 
  (do
    (swap! registry-ref assoc k codec)
    k))

(defmacro defcodecspec
  "Will def a given spec and codec with the keyword.  Specs and codecs can be passed as a vector
  or as seperate arguments"
  ([k [spec codec]] (binary-codec.core/defcodecspec ~k ~spec ~codec))
  ([k spec codec] `(do 
                     (s/def ~k ~spec)
                     (binary-codec.core/def ~k ~codec)
                     ~k)))

(defn encode [codec-or-k encoding]
   (if-let [codec (reg-resolve codec-or-k)]
     (encode codec encoding)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k)))))

(defn encoded? [codec-or-k]
   (if-let [codec (reg-resolve codec-or-k)]
     (encoded? codec)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k)))))

(defn alignment
  ([codec-or-k] (alignment codec-or-k encoding/base-encoding))
  ([codec-or-k encoding] 
   (if-let [codec (reg-resolve codec-or-k)]
     (alignment* codec encoding)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn sizeof 
  ([codec-or-k] (sizeof codec-or-k encoding/base-encoding nil))
  ([codec-or-k encoding] (sizeof codec-or-k encoding nil))
  ([codec-or-k encoding data]
   (if-let [codec (reg-resolve codec-or-k)]
     (sizeof* codec encoding data)
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn to-buffer!
  ([codec-or-k data buffer] (to-buffer! codec-or-k encoding/base-encoding data buffer))
  ([codec-or-k encoding data buffer]
   (if-let [codec (reg-resolve codec-or-k)]
     (do
       (align-buffer-write (alignment* codec encoding) buffer)
       (to-buffer!* codec encoding data buffer))
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))

(defn from-buffer!
  ([codec-or-k buffer] (from-buffer! codec-or-k encoding/base-encoding buffer))
  ([codec-or-k encoding buffer]
   (if-let [codec (reg-resolve codec-or-k)]
     (do
       (align-buffer-read (alignment* codec encoding) buffer)
       (from-buffer!* codec encoding buffer))
     (throw (Exception. (str "Unable to resolve codec - " codec-or-k))))))


(binary-codec.core/defcodecspec
  ::int8 
  (make-signed-integral-conformer byte)
  (reify Codec
    (encode* [_ encoding])
    (encoded?* [_] true)
    (alignment* [_ _] 0)
    (sizeof* [_ _ _] Byte/BYTES)
    (to-buffer!* [_ _ data buffer] (.put buffer data))
    (from-buffer!* [_ _ buffer] (.get buffer))))

(binary-codec.core/defcodecspec
  ::int16
  (make-signed-integral-conformer short)
  (reify Codec
    (encode* [_ encoding])
    (encoded?* [_] true)
    (alignment* [_ encoding] 
      (let [{word-size ::encoding/word-size} (s/conform ::encoding/base-encoding encoding)]
        (min word-size Short/BYTES)))
    (sizeof* [_ _ _] Short/BYTES)
    (to-buffer!* [_ _ data buffer] (.putShort buffer data))
    (from-buffer!* [_ _ buffer] (.getShort buffer))))

(binary-codec.core/defcodecspec 
  ::int32
  (make-signed-integral-conformer int)
  (reify Codec
    (encode* [_ encoding])
    (encoded?* [_] true)
    (alignment* [_ encoding]
      (let [{word-size ::encoding/word-size} (s/conform ::encoding/base-encoding encoding)]
        (min word-size Integer/BYTES)))
    (sizeof* [_ _ _] Integer/BYTES)
    (to-buffer!* [_ _ data buffer] (.putInt buffer data))
    (from-buffer!* [_ _ buffer] (.getInt buffer))))

(binary-codec.core/defcodecspec 
  ::int64
  (make-signed-integral-conformer long)
  (reify Codec
    (encode* [_ encoding])
    (encoded?* [_] true)
    (alignment* [_ encoding]
      (let [{word-size ::encoding/word-size} (s/conform ::encoding/base-encoding encoding)]
        (min word-size Long/BYTES)))
    (sizeof* [_ _ _] Long/BYTES)
    (to-buffer!* [_ _ data buffer] (.putLong buffer data))
    (from-buffer!* [_ _ buffer] (.getLong buffer))))

(binary-codec.core/defcodecspec ::uint8 (make-unsigned-integral-conformer Byte/SIZE byte unchecked-byte) ::int8)
(binary-codec.core/defcodecspec ::uint16 (make-unsigned-integral-conformer Short/SIZE short unchecked-short) ::int16)
(binary-codec.core/defcodecspec ::uint32 (make-unsigned-integral-conformer Integer/SIZE int unchecked-int) ::int32)

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

(defn tagged-union [base-codec dispatch]
  (reify Codec
    (alignment* [_ encoding] (alignment base-codec encoding))
    (sizeof* [_ encoding data] 
      (if (nil? data)
        nil
        (sizeof (dispatch data) encoding data)))
    (to-buffer!* [_ encoding data buffer] (to-buffer! (dispatch data) encoding data buffer))
    (from-buffer!* [_ encoding buffer]
      (let [base-data (from-buffer! base-codec encoding buffer)
            full-codec (dispatch base-data)
            base-read-length (sizeof base-codec encoding base-data)
            buffer-position (.position buffer)
            unwound-buffer (.position buffer (- buffer-position base-read-length))]
        (from-buffer! full-codec encoding buffer)))))


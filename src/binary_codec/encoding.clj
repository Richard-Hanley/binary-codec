(ns binary-codec.encoding
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [clojure.spec.alpha :as s]))


(def byte-orders {:endian/little ByteOrder/LITTLE_ENDIAN
                  :endian/big ByteOrder/BIG_ENDIAN
                  :endian/native (ByteOrder/nativeOrder)
                  :endian/network ByteOrder/BIG_ENDIAN})

(s/def ::byte-order (s/or :keyed-order (s/and keyword?
                                              (partial contains? byte-orders))
                          :raw-order (partial instance? ByteOrder)))
(s/def ::word-size #{1 2 4 8})
(s/def ::base-encoding (s/keys))

(defn buffer-op-with-endian [byte-order buffer-op buffer]
  (let [[order-type order] byte-order
        new-order (case order-type
                    :keyed-order (get byte-orders order)
                    :raw-order order)
        old-order (.order buffer)]
    (do
      (.order buffer new-order)
      (buffer-op buffer)
      (.order buffer old-order))))


(ns binary-codec.core-test
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [binary-codec.core :as codec :refer :all]))

(deftest test-integral-conformers
  (testing "signed conversions"
    (testing "int8"
      (testing "zero"
        (is (= Byte (type (s/conform ::codec/int8 0))))
        (is (= 0 (s/conform ::codec/int8 0))))
      (testing "middle value"
        (is (= Byte (type (s/conform ::codec/int8 15))))
        (is (= 15 (s/conform ::codec/int8 15))))
      (testing "max value"
        (is (= Byte (type (s/conform ::codec/int8 127))))
        (is (= 127 (s/conform ::codec/int8 127))))
      (testing "min value"
        (is (= Byte (type (s/conform ::codec/int8 -128))))
        (is (= -128 (s/conform ::codec/int8 -128))))
      (testing "invalid values"
        (is (= ::s/invalid (s/conform ::codec/int8 128)))
        (is (= ::s/invalid (s/conform ::codec/int8 -129)))
        (is (= ::s/invalid (s/conform ::codec/int8 256)))))
    (testing "int16"
      (testing "zero"
        (is (= Short (type (s/conform ::codec/int16 0))))
        (is (= 0 (s/conform ::codec/int16 0))))
      (testing "middle value"
        (is (= Short (type (s/conform ::codec/int16 15))))
        (is (= 15 (s/conform ::codec/int16 15))))
        (is (= Short (type (s/conform ::codec/int16 752))))
        (is (= 752 (s/conform ::codec/int16 752))))
      (testing "max value"
        (is (= Short (type (s/conform ::codec/int16 32767))))
        (is (= 32767 (s/conform ::codec/int16 32767))))
      (testing "min value"
        (is (= Short (type (s/conform ::codec/int16 -32768))))
        (is (= -32768 (s/conform ::codec/int16 -32768))))
      (testing "invalid values"
        (is (= ::s/invalid (s/conform ::codec/int16 0xFFFF)))
        (is (= ::s/invalid (s/conform ::codec/int16 0x10000)))
        (is (= ::s/invalid (s/conform ::codec/int16 32768)))
        (is (= ::s/invalid (s/conform ::codec/int16 -32769)))))
  (testing "unsigned conversions"
    (testing "uint8"
      (testing "zero"
        (is (= Byte (type (s/conform ::codec/uint8 0))))
        (is (= 0 (s/conform ::codec/uint8 0))))
      (testing "middle value"
        (is (= Byte (type (s/conform ::codec/uint8 15))))
        (is (= 15 (s/conform ::codec/uint8 15))))
      (testing "max value"
        (is (= Byte (type (s/conform ::codec/uint8 255))))
        (is (= -1 (s/conform ::codec/uint8 255))))
      (testing "min value"
        (is (= Byte (type (s/conform ::codec/uint8 -128))))
        (is (= -128 (s/conform ::codec/uint8 -128))))
      (testing "invalid values"
        (is (= ::s/invalid (s/conform ::codec/uint8 -129)))
        (is (= ::s/invalid (s/conform ::codec/uint8 256)))))
    (testing "uint16"
      (testing "zero"
        (is (= Short (type (s/conform ::codec/uint16 0))))
        (is (= 0 (s/conform ::codec/uint16 0))))
      (testing "middle value"
        (is (= Short (type (s/conform ::codec/uint16 15))))
        (is (= 15 (s/conform ::codec/uint16 15)))
        (is (= Short (type (s/conform ::codec/uint16 752))))
        (is (= 752 (s/conform ::codec/uint16 752))))
      (testing "max value"
        (is (= Short (type (s/conform ::codec/uint16 0xFFFF))))
        (is (= -1 (s/conform ::codec/uint16 0xFFFF))))
      (testing "min value"
        (is (= Short (type (s/conform ::codec/uint16 -32768))))
        (is (= -32768 (s/conform ::codec/uint16 -32768))))
      (testing "invalid values"
        (is (= ::s/invalid (s/conform ::codec/uint16 0x10000)))
        (is (= ::s/invalid (s/conform ::codec/uint16 -32769)))))
 (testing "uint32"
      (testing "zero"
        (is (= Integer (type (s/conform ::codec/uint32 0))))
        (is (= 0 (s/conform ::codec/uint32 0))))
      (testing "middle value"
        (is (= Integer (type (s/conform ::codec/uint32 15))))
        (is (= 15 (s/conform ::codec/uint32 15))))
        (is (= Integer (type (s/conform ::codec/uint32 752))))
        (is (= 752 (s/conform ::codec/uint32 752))))
      (testing "max value"
        (is (= Integer (type (s/conform ::codec/uint32 0xFFFFFFFF))))
        (is (= -1 (s/conform ::codec/uint32 0xFFFFFFFF))))
      (testing "min value"
        (is (= Integer (type (s/conform ::codec/uint32 -2147483648))))
        (is (= -2147483648 (s/conform ::codec/uint32 -2147483648))))
      (testing "invalid values"
        (is (= ::s/invalid (s/conform ::codec/uint32 0x100000000)))
        (is (= ::s/invalid (s/conform ::codec/uint32 -2147483649))))

))


(deftest test-numerical-sizes
  (is (= 1 (codec/sizeof ::codec/int8)))
  (is (= 2 (codec/sizeof ::codec/int16)))
  (is (= 4 (codec/sizeof ::codec/int32)))
  (is (= 8 (codec/sizeof ::codec/int64))))

(deftest test-numerical-alignment
  (testing "Base Encoding (no alignment)"
    (is (= 0 (codec/alignment ::codec/int8)))
    (is (= 0 (codec/alignment ::codec/int16)))
    (is (= 0 (codec/alignment ::codec/int32)))
    (is (= 0 (codec/alignment ::codec/int64))))
  (testing "Alignment specified (1-byte)"
    (is (= 0 (codec/alignment ::codec/int8 {::codec/word-size 1})))
    (is (= 1 (codec/alignment ::codec/int16 {::codec/word-size 1})))
    (is (= 1 (codec/alignment ::codec/int32 {::codec/word-size 1})))
    (is (= 1 (codec/alignment ::codec/int64 {::codec/word-size 1}))))
  (testing "Alignment specified (2-byte)"
    (is (= 0 (codec/alignment ::codec/int8 {::codec/word-size 2})))
    (is (= 2 (codec/alignment ::codec/int16 {::codec/word-size 2})))
    (is (= 2 (codec/alignment ::codec/int32 {::codec/word-size 2})))
    (is (= 2 (codec/alignment ::codec/int64 {::codec/word-size 2}))))
  (testing "Alignment specified (4-byte)"
    (is (= 0 (codec/alignment ::codec/int8 {::codec/word-size 4})))
    (is (= 2 (codec/alignment ::codec/int16 {::codec/word-size 4})))
    (is (= 4 (codec/alignment ::codec/int32 {::codec/word-size 4})))
    (is (= 4 (codec/alignment ::codec/int64 {::codec/word-size 4}))))
  (testing "Alignment specified (8-byte)"
    (is (= 0 (codec/alignment ::codec/int8 {::codec/word-size 8})))
    (is (= 2 (codec/alignment ::codec/int16 {::codec/word-size 8})))
    (is (= 4 (codec/alignment ::codec/int32 {::codec/word-size 8})))
    (is (= 8 (codec/alignment ::codec/int64 {::codec/word-size 8})))))

(deftest test-numerical-put
  (testing "Test Byte"
    (testing "Little Endian"
      (let [buffer (.flip (to-buffer! ::codec/int8 
                                      (byte 15) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/LITTLE_ENDIAN)))]
        (is (= 15 (from-buffer! ::codec/int8 buffer)))))
    (testing "Big Endian"
      (let [buffer (.flip (to-buffer! ::codec/int8 
                                      (byte 15) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/BIG_ENDIAN)))]
        (is (= 15 (from-buffer! ::codec/int8 buffer))))))
  (testing "Test Short"
    (testing "Little Endian"
      (let [buffer (.flip (to-buffer! ::codec/int16 
                                      (short 0x7F4) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/LITTLE_ENDIAN)))]
        (is (= 0x7F4 (from-buffer! ::codec/int16 buffer)))))
    (testing "Big Endian"
      (let [buffer (.flip (to-buffer! ::codec/int16 
                                      (short 0x7F4) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/BIG_ENDIAN)))]
        (is (= 0x7F4 (from-buffer! ::codec/int16 buffer))))))
  (testing "Test Integer"
    (testing "Little Endian"
      (let [buffer (.flip (to-buffer! ::codec/int32 
                                      (int 0x7890BEEF) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/LITTLE_ENDIAN)))]
        (is (= 0x7890BEEF (from-buffer! ::codec/int32 buffer)))))
    (testing "Big Endian"
      (let [buffer (.flip (to-buffer! ::codec/int32 
                                      (int 0x7890BEEF) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/BIG_ENDIAN)))]
        (is (= 0x7890BEEF (from-buffer! ::codec/int32 buffer))))))
  (testing "Test Long"
    (testing "Little Endian"
      (let [buffer (.flip (to-buffer! ::codec/int64 
                                      (long 0x31337DEADBEEF) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/LITTLE_ENDIAN)))]
        (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 buffer)))))
    (testing "Big Endian"
      (let [buffer (.flip (to-buffer! ::codec/int64 
                                      (long 0x31337DEADBEEF) 
                                      (.order (ByteBuffer/allocate 10) ByteOrder/BIG_ENDIAN)))]
        (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 buffer)))))))



(deftest test-aligned
  (let [encoding {::codec/word-size 8}
        fill-buffer-with-offset (fn [offset codec value] 
                                  (let [buffer (ByteBuffer/allocate 40)]
                                    (dotimes [n offset] (.put buffer (byte 0)))
                                    (to-buffer! codec encoding value buffer)
                                    (.flip buffer)))]
    (testing "Alignment off by 1"
      (testing "int16"
        (let [buffer (fill-buffer-with-offset 1 ::codec/int16 (short 0x7F4))]
          (is (= 0 (from-buffer! ::codec/int16 encoding buffer)))
          (is (= 0x7F4 (from-buffer! ::codec/int16 encoding buffer)))))
      (testing "int32"
        (let [buffer (fill-buffer-with-offset 1 ::codec/int32 (int 0x7890BEEF))]
          (is (= 0 (from-buffer! ::codec/int32 encoding buffer)))
          (is (= 0x7890BEEF (from-buffer! ::codec/int32 encoding buffer)))))
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 1 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 2"
      (testing "int32"
        (let [buffer (fill-buffer-with-offset 2 ::codec/int32 (int 0x7890BEEF))]
          (is (= 0 (from-buffer! ::codec/int32 encoding buffer)))
          (is (= 0x7890BEEF (from-buffer! ::codec/int32 encoding buffer)))))
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 2 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 3"
      (testing "int32"
        (let [buffer (fill-buffer-with-offset 3 ::codec/int32 (int 0x7890BEEF))]
          (is (= 0 (from-buffer! ::codec/int32 encoding buffer)))
          (is (= 0x7890BEEF (from-buffer! ::codec/int32 encoding buffer)))))
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 3 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 4"
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 4 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 5"
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 5 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 6"
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 6 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    (testing "Alignment off by 7"
      (testing "int64"
        (let [buffer (fill-buffer-with-offset 7 ::codec/int64 (long 0x31337DEADBEEF))]
          (is (= 0 (from-buffer! ::codec/int64 encoding buffer)))
          (is (= 0x31337DEADBEEF (from-buffer! ::codec/int64 encoding buffer))))))
    ))

(codec/def ::tfoo [::codec/int8 ::codec/int64 ::codec/int16])
(codec/def ::mfoo {::bar ::codec/int8 ::baz ::codec/int64 ::bane ::codec/int16})

(deftest test-seq
  (testing "alignment"
    (testing "1 byte" (is (= 1 (alignment ::tfoo {::codec/word-size 1}))))
    (testing "2 byte" (is (= 2 (alignment ::tfoo {::codec/word-size 2}))))
    (testing "4 byte" (is (= 4 (alignment ::tfoo {::codec/word-size 4}))))
    (testing "8 byte" (is (= 8 (alignment ::tfoo {::codec/word-size 8})))))
  (testing "sizeof"
    (testing "unaligned" (is (= 11 (sizeof ::tfoo))))
    (testing "1 byte alignment" (is (= 11 (sizeof ::tfoo {::codec/word-size 1}))))
    (testing "2 byte alignment" (is (= 12 (sizeof ::tfoo {::codec/word-size 2}))))
    (testing "4 byte alignment" (is (= 14 (sizeof ::tfoo {::codec/word-size 4}))))
    (testing "8 byte alignment" (is (= 18 (sizeof ::tfoo {::codec/word-size 8})))))
  (testing "buffer writing and reading"
    (let [data [(byte 25) (long 0x31337DEADBEEF) (short 754)]
          buffer (.flip 
                   (to-buffer! ::tfoo codec/base-encoding data (ByteBuffer/allocate 40)))]
      (is (= data (from-buffer! ::tfoo codec/base-encoding buffer))))))

(deftest test-keys
    (let [data {::bane (short 754) ::baz (long 0x31337DEADBEEF) ::bar (byte 15)}
          buffer (.flip 
                   (to-buffer! ::mfoo codec/base-encoding data (ByteBuffer/allocate 40)))]
      (is (= data (from-buffer! ::mfoo codec/base-encoding buffer)))))

(codec/def ::base-foo {::length ::codec/uint8 ::type ::codec/uint8})
(codec/def ::fooa {::length ::codec/uint8 ::type ::codec/uint8 ::a ::codec/uint8})
(codec/def ::foob {::length ::codec/uint8 ::type ::codec/uint8 ::b ::codec/uint16})
(codec/def ::fooc {::length ::codec/uint8 ::type ::codec/uint8 ::c ::codec/uint32})
(codec/def ::food {::length ::codec/uint8 ::type ::codec/uint8 ::d ::codec/int64})

(def foo-type {1 ::fooa 2 ::foob 3 ::fooc 4 ::food})
(codec/def ::ufoo (codec/tagged-union ::base-foo (fn [foo] (get foo-type (::type foo)))))

(deftest test-union
  (testing "sizeof without data" (is (= nil (codec/sizeof ::ufoo))))
  (testing "fooa"
    (let [fooa {::length (byte 3) ::type (byte 1) ::a (byte 17)}
          buff (to-buffer! ::ufoo fooa (ByteBuffer/allocate 20))
          data (from-buffer! ::ufoo (.flip buff))]
      (testing "sizeof" (is (= 3 (codec/sizeof ::ufoo codec/base-encoding fooa))))
      (testing "encoding" (is (= fooa data)))))
  (testing "foob"
    (let [foob {::length (byte 4) ::type (byte 2) ::b (short 257)}
          buff (to-buffer! ::ufoo foob (ByteBuffer/allocate 20))
          data (from-buffer! ::ufoo (.flip buff))]
      (testing "sizeof" (is (= 4 (codec/sizeof ::ufoo codec/base-encoding foob))))
      (testing "encoding" (is (= foob data)))))
  (testing "fooc"
    (let [fooc {::length (byte 6) ::type (byte 3) ::c (unchecked-int 0xDEADBEEF)}
          buff (to-buffer! ::ufoo fooc (ByteBuffer/allocate 20))
          data (from-buffer! ::ufoo (.flip buff))]
      (testing "sizeof" (is (= 6 (codec/sizeof ::ufoo codec/base-encoding fooc))))
      (testing "encoding" (is (= fooc data)))))
  (testing "food"
    (let [food {::length (byte 10) ::type (byte 4) ::d (long 0xDEADBEEF)}
          buff (to-buffer! ::ufoo food (ByteBuffer/allocate 20))
          data (from-buffer! ::ufoo (.flip buff))]
      (testing "sizeof" (is (= 10 (codec/sizeof ::ufoo codec/base-encoding food))))
      (testing "encoding" (is (= food data))))))

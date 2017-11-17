(ns binary-codec.core-test
  (:import (java.nio ByteBuffer
                     ByteOrder))
  (:require [clojure.test :refer :all]
            [binary-codec.core :as codec :refer :all]))

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


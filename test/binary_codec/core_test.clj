(ns binary-codec.core-test
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

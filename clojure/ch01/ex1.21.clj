(ns sicp.ch01 (:use clojure.contrib.test-is))

(defn square [n] (* n n))

(defn smallest-divisor [n] (find-divisor n 2))

(defn divides? [test-divisor n] (= (rem n test-divisor) 0))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n  (+ test-divisor 1))))

(defn prime? [n] (= (smallest-divisor n) n))

(deftest test-prime
  (is (= true  (prime? 199)))
  (is (= true  (prime? 1999)))  
  (is (= false (prime? 19999))))

(deftest test-smallest-divisor
  (is (= 199  (smallest-divisor 199)))
  (is (= 1999 (smallest-divisor 1999)))  
  (is (= 7    (smallest-divisor 19999))))

; Stack overflow? TODO
; (run-tests)  

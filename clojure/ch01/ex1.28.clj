(ns sicp.ch01 (:use clojure.test))

; Miller-Rabin Test

(defn square [n] (* n n))

(defn mr-test [base exp m]
  (cond (= base 1) false
        (= base (- m 1)) false
        :else (= (square base) (rem 1 m))))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp)
          (cond (not (mr-test base exp m)) 0
          :else (rem (square (expmod base (/ exp 2) m)) m))
        :else 
          (rem (* base (expmod base (- exp 1) m)) m)))
          
(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int (- n 1)))))

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (- times 1))
        :else false))

(deftest test-miller-rabin-prime-test
  (is (= false (fast-prime? 561  1000)))
  (is (= false (fast-prime? 1105 1000)))
  (is (= false (fast-prime? 1729 1000)))
  (is (= false (fast-prime? 2465 1000)))
  (is (= false (fast-prime? 2821 1000)))
  (is (= false (fast-prime? 6601 1000))))

(run-tests)

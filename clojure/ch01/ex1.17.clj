(ns sicp.ch01 (:use clojure.contrib.test-is))

(defn double2 "Because clojure already defines 'double'." [n] (* n 2))
(defn halve [n]  (/ n 2))
(defn mult [a n]
  (defn mult2 [b m]
    (cond (= m 0) 0
          (even? m) (mult2 (double2 b) (halve m))
          :else (+ b (mult2 b (- m 1)))))
  (mult2 a n))

(deftest test-mult
  (is (=  0 (mult 2 0)))
  (is (=  2 (mult 2 1)))
  (is (=  4 (mult 2 2)))
  (is (=  6 (mult 2 3)))
  (is (=  8 (mult 2 4)))
  (is (= 10 (mult 2 5)))
  (is (= 12 (mult 2 6)))
  (is (= 14 (mult 2 7))))

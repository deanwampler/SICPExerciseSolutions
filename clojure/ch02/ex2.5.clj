(ns sicp.ch02 (:use clojure.test))
(use '[clojure.contrib.except :only (throw-if)])

;(defn even? [n] (= 0 (rem n 2)))
(defn square [x] (* x x))
(defn fast-expt [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (- n 1)))))

; To find a, keep dividing z = (cons a b) by 2, counting the number of times.
(defn find-a [z]
  (defn f [a left-over]
    (cond (= (rem left-over 2) 0) (f (+ a 1) (/ left-over 2))
          :else a))
  (f 0 z))

; To find b, keep dividing z = (cons a b) by 3, counting the number of times.
(defn find-b [z]
  (defn f [b left-over]
    (cond (= (rem left-over 3) 0) (f (+ b 1) (/ left-over 3))
          :else b))
  (f 0 z))

(defn cons2 [a b]
  (* (fast-expt 2 a) (fast-expt 3 b)))

(defn car [z] (find-a z))
(defn cdr [z] (find-b z))

(deftest test-exp-pair
  (def zero-zero (cons2 0 0))
  (def one-two   (cons2 1 2))
  (def two-one   (cons2 2 1))
  (def four-five (cons2 4 5))
  (is (= (car zero-zero) 0))
  (is (= (cdr zero-zero) 0))
  (is (= (car one-two)   1))
  (is (= (cdr one-two)   2))
  (is (= (car two-one)   2))
  (is (= (cdr two-one)   1))
  (is (= (car four-five) 4))
  (is (= (cdr four-five) 5)))

(run-tests)

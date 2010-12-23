(ns sicp.ch02 (:use clojure.test))

(defn make-interval [a b] [a b])
(defn lower-bound [x] (get x 0))
(defn upper-bound [x] (get x 1))

; For subtraction, subtract the upper bound of y from the lower bound of x and
; the lower bound of y from the upper bound of x. The reason is that if the "real"
; value of y is actually towards the upper bounds, then subtracting y from x 
; will yield a lower number than if y is near its lower bound. Hence, we want the
; new lower bound to use the upper bound of y. A similar argument apples to the 
; new upper bound. The highest the new upper bound can be is the value of x's 
; upper bound minus y's lower bound.

(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
                                 
(def minus-one-one-interval (make-interval -1.0 1.0))
(def zero-one-interval      (make-interval  0.0 1.0))
(def two-three-interval     (make-interval  2.0 3.0))

(def sub1 (sub-interval minus-one-one-interval zero-one-interval))
(def sub2 (sub-interval minus-one-one-interval two-three-interval))
(def sub3 (sub-interval zero-one-interval      two-three-interval))

(deftest test-interval
  (is (= (lower-bound sub1) -2.0))
  (is (= (upper-bound sub1)  1.0))
  (is (= (lower-bound sub2) -4.0))
  (is (= (upper-bound sub2) -1.0))
  (is (= (lower-bound sub3) -3.0))
  (is (= (upper-bound sub3) -1.0)))
(run-tests)
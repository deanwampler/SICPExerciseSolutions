; Newton's Method for Cubes, using the approach in 1.7.
(ns sicp.ch01 (:use clojure.test))

(defn cube [x] (* x x x))

(defn improve [guess x]
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  
(defn abs [x]
  (if (> x 0) x (- x)))

(defn good-enough? [guess prev-guess x]
  (< (abs (- guess prev-guess)) (* guess 0.00001)))
  
(defn cube-root-iter [guess prev-guess x]
  (if (good-enough? guess prev-guess x)
    guess
    (cube-root-iter (improve guess x) guess x)))

(defn cube-root [x]
  (cube-root-iter 1.0 0.0 x))

(deftest test-cube-root
  (is (< (abs (- (cube-root  8E15) 2E5)) 1.0))
  (is (< (abs (- (cube-root  8E-15) 2E-5)) 1.0)))

(run-tests)

(ns sicp.ch01 (:use clojure.contrib.test-is))

; The original implementation in the book:

(defn square [x] (* x x))

(defn average [x y]
  (/ (+ x y) 2))
  
(defn abs [x]
  (if (> x 0) x (- x)))
  
(defn improve [guess x]
  (average guess (/ x guess)))
  
(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.00001))
  
(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))
  
; Refined implementation

(defn good-enough2? [guess prev-guess x]
  (< (abs (- guess prev-guess)) (* guess 0.00001)))
  
(defn sqrt-iter2 [guess prev-guess x]
  (if (good-enough2? guess prev-guess x)
    guess
    (sqrt-iter2 (improve guess x) guess x)))

(defn sqrt2 [x]
  (sqrt-iter2 1.0 0.0 x))

(deftest test-improved-sqrt
  ; The original sqrt is actually more accurate for large X, if we use a larger
  ; delta (0.001) in the good-enough? and good-enough2? methods. With 0.00001, 
  ; they are about the same.
  (is (< (abs (- (sqrt  4E16) 2E8)) 1.0))
  (is (< (abs (- (sqrt2 4E16) 2E8)) 1.0))
  ; For very small X, the original sqrt is horrible; this test fails
  ; (is (< (abs (- (sqrt  4E-16) 2E-8)) 0.002)) ; note the error range!
  (is (< (abs (- (sqrt2 4E-16) 2E-8)) 1E-12)))

(run-tests)

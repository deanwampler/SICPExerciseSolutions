(ns sicp.ch01 (:use clojure.contrib.test-is))

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))
    
(defn inc [n] (+ n 1))
(defn even? [n] (= 0 (rem n 2)))

(defn simpsons-rule [f a b n]
  (def h (/ (- b a) n))
  (defn fy [k] (f (+ a (* k h))))
  (defn term [i]
    (* (fy i) (if (even? i) 2 4)))
  (* (/ h 3) (+ (fy 0) (sum term 1 inc (- n 1)) (fy n))))
  
(defn cube [n] (* n n n))

(defn abs [x] (if (> x 0.0) x (- x)))

(deftest test-simpsons-rule
  (is (> 0.001 (abs (- 0.25 (simpsons-rule cube 0.0 1.0 10)))))
  (is (> 0.001 (abs (- 0.25 (simpsons-rule cube 0.0 1.0 100)))))
  (is (> 0.001 (abs (- 0.25 (simpsons-rule cube 0.0 1.0 1000)))))
  ; Appears to stack overflow; see next problem that uses tail recursion
  ;(is (> 0.001 (abs (- 0.25 (simpsons-rule cube 0.0 1.0 10000)))))
  )

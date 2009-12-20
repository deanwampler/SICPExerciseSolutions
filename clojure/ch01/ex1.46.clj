(ns sicp.ch01 (:use clojure.test))

(defn iterative-improve [good-enough? improve]
  (fn [guess] 
    (loop [guess2 guess]
      (if (good-enough? guess2)
        guess2
        (recur (improve guess2))))))

(defn square [x] (* x x))

(defn average [x y]
  (/ (+ x y) 2))

(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x)) 0.001))

(defn improve [guess x]
  (average guess (/ x guess)))
  
(defn sqrt [x]
  ((iterative-improve
    #(good-enough? % x)
    #(improve % x)) 1.0))
    
; Is there a method like this in test-is?? Couldn't find something like it documented.
(defn is-within [expected actual delta]
  (< (Math/abs (- expected actual)) delta))

(deftest test-iterative-improve-for-sqrt
  (is (is-within 1.414213562373095 (sqrt 2)    0.0001))
  (is (is-within 1.732050807568877 (sqrt 3)    0.0001))
  (is (is-within 2.0               (sqrt 4)    0.0001))
  (is (is-within 2E8               (sqrt 4E16) 1.0))
  ; For very small X, the original sqrt is horrible; this test fails
  ; (check-= (sqrt 4E-16) 2E-8 1E-12)
  )
(run-tests)
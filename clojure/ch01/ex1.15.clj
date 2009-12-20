(ns sicp.ch01 (:use clojure.test))

(defn cube [x] (* x x x))

(def num-iterations (atom 0))
(defn p [x]
  (reset! num-iterations (+ @num-iterations 1))
  (- (* 3 x) (* 4 (cube x))))

(defn sine [angle]
  (if (< (Math/abs angle) 0.1) 
    angle 
    (p (sine (/ angle 3)))))

(sine 12.15)

(deftest test-sine-num-iterations
  (is (= 5 @num-iterations)))

(run-tests)

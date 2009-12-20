(ns sicp.ch01 (:use clojure.test))

; Fixed point, again

(def tolerance 0.00001)

(defn abs [x] (if (> x 0) x (- x)))

(defn close-enough? [x y]
  (< (abs (- x y)) tolerance))

(defn average [x y]
  (/ (+ x y) 2.0))

(defn fixed-point [f first-guess]
  (loop [guess first-guess count 1]
    (let [next-guess (f guess)]
      (cond (close-enough? guess next-guess) 
              (do (println count) next-guess)
            :else (recur next-guess (+ count 1))))))

; Is there a method like this in test-is?? Couldn't find something like it documented.
(defn is-within [expected actual delta]
  (< (abs (- expected actual)) delta))
  
(deftest test-fixed-point
  (is (= true (is-within 
    (fixed-point (fn [x] (/ (Math/log 1000) (Math/log x))) 10.0) 4.55553 tolerance)))
  (is (= true (is-within 
    (fixed-point (fn [x] (average x (/ (Math/log 1000) (Math/log x)))) 10.0) 4.55553 tolerance))))

(run-tests)

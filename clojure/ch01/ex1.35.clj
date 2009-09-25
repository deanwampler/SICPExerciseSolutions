(ns sicp.ch01 (:use clojure.contrib.test-is))

; Calculate the golden ratio, phi.

(def tolerance 0.00001)

(defn abs [x] (if (> x 0) x (- x)))

(defn close-enough? [x y]
  (< (abs (- x y)) tolerance))

(defn average [x y]
  (/ (+ x y) 2.0))

(defn fixed-point [f first-guess]
  (defn fp [guess]
    (let [next-guess (f guess)]
      (cond (close-enough? guess next-guess) next-guess
            :else (fp next-guess))))
  (fp first-guess))

(deftest test-fixed-point
  (is (= 1.6180327868852458 (fixed-point (fn [x] (+ 1.0 (/ 1.0 x))) 1.0))))

(run-tests)

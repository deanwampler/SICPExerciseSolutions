(ns sicp.ch01 (:use clojure.contrib.test-is))

(def tolerance 0.00001)

(defn close-enough? [x y]
  (< (Math/abs (- x y)) tolerance))  ; book uses 0.001 for the half-interval discussion.

(defn average [x y]
  (/ (+ x y) 2))

(defn fixed-point [f first-guess]
  (loop [guess first-guess count 1]
    (let [next (f guess)]
      (if (close-enough? guess next)
        next
        (recur next (+ count 1))))))  

(defn average-damp [f]
  (fn [x] (average x (f x))))

(defn average-nth-order-damp [n f]
  (defn anod [i]
    (cond (= i 0) f
          :else (average-damp (anod (- i 1)))))
  (anod n))

(defn nth-power [x n]
  (cond (= n 0) 1
        :else (* x (nth-power x (- n 1)))))

(deftest test-nth-power
  (is (= (nth-power 2 0) 1))
  (is (= (nth-power 2 1) 2))
  (is (= (nth-power 2 2) 4))
  (is (= (nth-power 2 3) 8))
  (is (= (nth-power 2 4) 16))
  (is (= (nth-power 2 5) 32))
  (is (= (nth-power 3 1) 3))
  (is (= (nth-power 3 2) 9))
  (is (= (nth-power 3 3) 27)))
(run-tests)

(defn averaged-x-over-y-n1 [damp-count n]
  (fn [x]
    (average-nth-order-damp damp-count 
      (fn [y] (/ x (nth-power y (- n 1)))))))

(defn nth-root [x n damp-count]
  (fixed-point ((averaged-x-over-y-n1 damp-count n) x) 1.1))

(defn try-it [root n damp-count]
  (println (format "damp count %d, nth root = %f, root^n = %f" damp-count root (nth-power root n))))

(println "square root 2.0 = 1.414213562373095")
(try-it (nth-root 2.0 2 1) 2 1)
(try-it (nth-root 2.0 2 2) 2 2)
(try-it (nth-root 2.0 2 3) 2 3)
(try-it (nth-root 2.0 2 4) 2 4)

(println"Cube root for 2.0 = 1.259921049894873")
(try-it (nth-root 2.0 3 2) 3 2)
(try-it (nth-root 2.0 3 3) 3 3)
(try-it (nth-root 2.0 3 4) 3 4)

(println"4th root for 2.0 = 1.189207115002721")
(try-it (nth-root 2.0 4 3) 4 3)
(try-it (nth-root 2.0 4 4) 4 4)
(try-it (nth-root 2.0 4 5) 4 5)

(println"5th root for 2.0 = 1.148698354997035")
(try-it (nth-root 2.0 5 4) 5 4)
(try-it (nth-root 2.0 5 5) 5 5)
(try-it (nth-root 2.0 5 6) 5 6)

(println"6th root for 2.0 = 1.122462048309373")
(try-it (nth-root 2.0 6 5) 6 5)
(try-it (nth-root 2.0 6 6) 6 6)
(try-it (nth-root 2.0 6 7) 6 7)

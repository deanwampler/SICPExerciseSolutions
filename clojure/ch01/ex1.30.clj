(ns sicp.ch01 (:use clojure.test))

; tail recursive
(defn sum [term a next b]
  (loop [a2 a accum 0.0]
    (if (> a2 b)
      accum
      (recur (next a2) (+ accum (term a2))))))

(defn increment [n] (+ n 1))
(defn is-even? [n] (= 0 (rem n 2)))

(defn simpsons-rule2 [f a b n]
  (def h (/ (- b a) n))
  (defn fy [k] (f (+ a (* k h))))
  (defn term [i]
    (* (fy i) (if (is-even? i) 2 4)))
  (* (/ h 3) (+ (fy 0) (sum term 1 increment (- n 1)) (fy n))))

(defn cube [n] (* n n n))
(defn abs [n] (if (> n 0) n (- n)))

(deftest test-simpsons-rule2
  (is (> 0.001 (abs (- 0.25 (simpsons-rule2 cube 0.0 1.0 10)))))
  (is (> 0.001 (abs (- 0.25 (simpsons-rule2 cube 0.0 1.0 100)))))
  (is (> 0.001 (abs (- 0.25 (simpsons-rule2 cube 0.0 1.0 1000)))))
  (is (> 0.001 (abs (- 0.25 (simpsons-rule2 cube 0.0 1.0 10000))))))

(run-tests)

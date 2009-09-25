(ns sicp.ch01 (:use clojure.contrib.test-is))

; Carmichael numbers

(defn square [n] (* n n))

(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp)
          (rem (square (expmod base (/ exp 2) m))
            m)
        :else 
          (rem (* base (expmod base (- exp 1) m))
            m)))

(defn congruent [n]
  (defn c [n a]
    (cond (= a n) true
          (= (expmod a n n) (rem a n)) (c n (+ a 1))
          :else false))
  (c n 1))
          
(deftest test-carmichael-numbers
  (is (= true (congruent 561)))
  (is (= true (congruent 1105)))
  (is (= true (congruent 1729)))
  (is (= true (congruent 2465)))
  (is (= true (congruent 2821)))
  (is (= true (congruent 6601))))

(run-tests)

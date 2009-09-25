(ns sicp.ch01 (:use clojure.contrib.test-is))

(defn square [x] (* x x))
(defn fast-expt [x n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt x (/ n 2)))
        :else (* x (fast-expt x (- n 1)))))

(deftest test-sine-num-iterations
  (is (= 2  (fast-expt 2 1)))
  (is (= 4  (fast-expt 2 2)))
  (is (= 8  (fast-expt 2 3)))
  (is (= 16 (fast-expt 2 4)))
  (is (= 32 (fast-expt 2 5)))
  (is (= 64 (fast-expt 2 6))))

; tail recursive version
(defn fast-expt2 [x n]
  (defn f [z m]
    (cond (= m 0) z
        (even? m) (f (* z (square x)) (- m 2))
        :else (f (* z x) (- m 1))))
  (f 1 n))
        
(deftest test-sine-num-iterations2
  (is (= 2  (fast-expt2 2 1)))
  (is (= 4  (fast-expt2 2 2)))
  (is (= 8  (fast-expt2 2 3)))
  (is (= 16 (fast-expt2 2 4)))
  (is (= 32 (fast-expt2 2 5)))
  (is (= 64 (fast-expt2 2 6))))

(run-tests)

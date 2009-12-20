(ns sicp.ch02 (:use clojure.test))

(defn both [op x y] (and (op x) (op y)))

(defn same-parity [p & l]
  (defn sm [l2 answer]
    (cond (empty? l2) answer
          (or (both even? p (first l2)) (both odd? p (first l2))) (sm (next l2) (cons (first l2) answer))
          :else (sm (next l2) answer)))
  (reverse (sm l (list p))))

(deftest test-same-parity
  (is (= (same-parity 1) '(1)))
  (is (= (same-parity 2) '(2)))
  (is (= (same-parity 1 2 3 4 5 6 7) '(1 3 5 7)))
  (is (= (same-parity 2 3 4 5 6 7) '(2 4 6)))
  (is (= (same-parity 3 4 5 6 7) '(3 5 7)))
  (is (= (same-parity 4 5 6 7) '(4 6))))

(run-tests)
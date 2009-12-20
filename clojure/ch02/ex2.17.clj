(ns sicp.ch02 (:use clojure.test))

(defn last-pair [l]
  (defn lp [l pair]
    (cond (empty? l) pair
          :else (lp (next l) (list (first (next pair)) (first l)))))
  (cond (nil? (next l)) (throw (Exception. "List too short"))
        :else (lp (next (next l)) (list (first l) (first (next l))))))

(deftest test-last-pair      
  (is (= (last-pair (list 1 2 3 4 5 6)) (list 5 6)))
  (is (= (last-pair (list 1 2 3)) (list 2 3)))
  (is (= (last-pair (list 1 2)) (list 1 2))))
  
(run-tests)

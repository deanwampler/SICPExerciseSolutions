(ns sicp.ch02 (:use clojure.contrib.test-is))

; reverse is already defined:
(defn reverse2 [l]
  (loop [l2 l result '()]
    (cond (empty? l2) result
          :else (recur (next l2) (cons (first l2) result)))))

(deftest test-reverse2      
  (is (= (reverse2 (list 1 2 3 4 5 6)) (list 6 5 4 3 2 1)))
  (is (= (reverse2 (list 1 2)) (list 2 1)))
  (is (= (reverse2 (list 1)) (list 1)))
  (is (= (reverse2 (list)) (list))))
  
(run-tests)

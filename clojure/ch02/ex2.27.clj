(ns sicp.ch02 (:use clojure.test))

; reverse is already defined:
(defn reverse2 [l]
  (loop [l2 l result '()]
    (cond (empty? l2) result
          :else (recur (next l2) (cons (first l2) result)))))

(defn deep-reverse [l]
  (loop [l2 l result '()]
    (cond (empty? l2) result
          (list? (first l2)) (recur (next l2) (cons (deep-reverse (first l2)) result))
          :else (recur (next l2) (cons (first l2) result)))))

(deftest test-deep-reverse      
  (is (= (reverse2     (list (list 1 2) (list 3 4))) (list (list 3 4) (list 1 2))))
  (is (= (deep-reverse (list (list 1 2) (list 3 4))) (list (list 4 3) (list 2 1))))
  (is (= (deep-reverse (list 10 (list 1 2) 20 (list 3 4) 30)) (list 30 (list 4 3) 20 (list 2 1) 10)))
  (is (= (deep-reverse (list 1 2 3 4 5 6)) (list 6 5 4 3 2 1)))
  (is (= (deep-reverse (list 1 2)) (list 2 1)))
  (is (= (deep-reverse (list 1)) (list 1)))
  (is (= (deep-reverse (list)) (list))))  
    
(run-tests)

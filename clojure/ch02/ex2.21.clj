(ns sicp.ch02 (:use clojure.test))

(defn square-list1 [items]
  (if (empty? items)
      '()
      (cons (* (first items) (first items)) (square-list1 (rest items)))))
      
(defn square-list2 [items]
  (map (fn [x] (* x x)) items))

(deftest test-square-list-variants
  (is (= (square-list1 (list 1 2 3 4 5)) (list 1 4 9 16 25)))
  (is (= (square-list2 (list 1 2 3 4 5)) (list 1 4 9 16 25))))
  
(run-tests)

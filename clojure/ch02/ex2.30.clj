(ns sicp.ch02 (:use clojure.test))

(defn square-tree1 [tree]
  (cond (list? tree)
        (if (empty? tree)
            '()
            (cons (square-tree1 (first tree)) (square-tree1 (rest tree))))
        :else (* tree tree)))
      
(defn square-tree2 [tree]
  (map (fn [x] 
    (if (list? x)
        (square-tree2 x)
        (* x x)))
    tree))

(deftest test-square-tree-variants
  (is (= (square-tree1 
      (list 1 (list 2 (list 3 4) 5) (list 6 7))) 
      (list 1 (list 4 (list 9 16) 25) (list 36 49))))
  (is (= (square-tree1 (list)) (list)))
  (is (= (square-tree1 (list 2)) (list 4)))
  (is (= (square-tree2 
      (list 1 (list 2 (list 3 4) 5) (list 6 7))) 
      (list 1 (list 4 (list 9 16) 25) (list 36 49)))))
  (is (= (square-tree2 (list)) (list)))
  (is (= (square-tree2 (list 2)) (list 4)))
  
(run-tests)

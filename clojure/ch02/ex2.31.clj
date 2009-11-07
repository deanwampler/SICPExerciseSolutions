(ns sicp.ch02 (:use clojure.contrib.test-is))

(defn tree-map [f tree]
  (cond (list? tree)
        (if (empty? tree)
            '()
            (cons (tree-map f (first tree)) (tree-map f (rest tree))))
        :else (f tree)))
      
(defn tree-map [f tree]
  (map (fn [x] 
    (if (list? x)
        (tree-map f x)
        (f x)))
    tree))

(defn square-tree [tree]
  (tree-map (fn [x] (* x x)) tree))
  
(deftest test-tree-map
  (is (= (square-tree 
      (list 1 (list 2 (list 3 4) 5) (list 6 7))) 
      (list 1 (list 4 (list 9 16) 25) (list 36 49))))
  (is (= (square-tree (list)) (list)))
  (is (= (square-tree (list 2)) (list 4))))
  
(run-tests)

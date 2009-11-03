(ns sicp.ch02 (:use clojure.contrib.test-is))

(deftest first-rest-test
  (is (= (first (rest (first (rest (rest (list 1 3 (list 5 7) 9)))))) 7))
  (is (= (first (first (list (list 7)))) 7))
  (is (= (first (rest (first (rest (first (rest (first (rest (first (rest (first (rest 
    (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))) 7)))
    
(run-tests)

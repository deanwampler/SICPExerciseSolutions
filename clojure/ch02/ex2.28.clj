(ns sicp.ch02 (:use clojure.test))

; This implementation using lists is awkward, due to the way "into" works. It's
; more straight forward if vectors are used.
(defn fringe [l]
  (loop [l2 l result '()]
    (cond (empty? l2) result
          (list? (first l2)) (recur (next l2) (into (fringe (first l2)) (reverse result)))
          :else (recur (next l2) (into (list (first l2)) (reverse result))))))

(deftest test-fringe      
  (is (= (fringe (list (list 1 2) (list 3 4))) (list 1 2 3 4)))
  (is (= (fringe (list 10 (list 1 2) 20 (list 3 4) 30)) (list 10 1 2 20 3 4 30)))
  (is (= (fringe (list 1 2 3 4 5 6)) (list 1 2 3 4 5 6)))
  (is (= (fringe (list 1 2)) (list 1 2)))
  (is (= (fringe (list 1)) (list 1)))
  (is (= (fringe (list)) (list))))  

; using vectors.
(defn fringe-vector [v]
  (loop [v2 v result []]
    (cond (empty? v2) result
          (vector? (first v2)) (recur (next v2) (into result (fringe-vector (first v2))))
          :else (recur (next v2) (into result [(first v2)])))))

(deftest test-fringe-vector      
  (is (= (fringe-vector [[1 2] [3 4]]) [1 2 3 4]))
  (is (= (fringe-vector [10 [1 2] 20 [3 4] 30]) [10 1 2 20 3 4 30]))
  (is (= (fringe-vector [1 2 3 4 5 6]) [1 2 3 4 5 6]))
  (is (= (fringe-vector [1 2]) [1 2]))
  (is (= (fringe-vector [1]) [1]))
  (is (= (fringe-vector []) [])))  
    
(run-tests)

(ns sicp.ch02 (:use clojure.contrib.test-is))

; Using vectors because they work better.

; Recall that accumulate is really fold-right
(defn fold-right [op initial sequence]
  (if (empty? sequence)
      initial
      (op (first sequence)
          (fold-right op initial (vec (rest sequence))))))

(defn flatmap [proc seq]
  (vec (fold-right into [] (vec (map proc seq)))))
  
(defn enumerate-interval [start end]
  (seq (loop [result [] n start]
        (if (> n end)
        result
        (recur (conj result n) (+ n 1))))))

(defn unique-pairs [n]
  (flatmap 
    (fn [i]
      (vec (map (fn [j] [i j])
        (enumerate-interval 1 (- i 1)))))
    (vec (enumerate-interval 1 n))))

(defn unique-triplets [n]
  (flatmap 
    (fn [k] (vec (map (fn [tuple-1] (vec (cons k tuple-1))) (unique-pairs (- k 1)))))
    (vec (enumerate-interval 1 n))))

(deftest test-unique-triplets
  (is (= (unique-triplets 6) 
          [[3 2 1] 
           [4 2 1] [4 3 1] [4 3 2] 
           [5 2 1] [5 3 1] [5 3 2] [5 4 1] [5 4 2] [5 4 3]
           [6 2 1] [6 3 1] [6 3 2] [6 4 1] [6 4 2] [6 4 3]
           [6 5 1] [6 5 2] [6 5 3] [6 5 4]])))

; Let's generalize this to arbitrary tuples:

(defn unique-tuple [arity n]
  (if (= arity 1)
      (vec (map (fn [x] [x]) (enumerate-interval 1 n)))
      (flatmap 
        (fn [k] (vec (map (fn [pair] (cons k pair)) (unique-tuple (- arity 1) (- k 1)))))
        (vec (enumerate-interval 1 n)))))

(deftest test-unique-tuple
  (is (= (unique-tuple 0 6) []))

  (is (= (unique-tuple 1 6)
          [[1] [2] [3] [4] [5] [6]]))

  (is (= (unique-tuple 2 6) 
          [[2 1] 
           [3 1] [3 2]
           [4 1] [4 2] [4 3]
           [5 1] [5 2] [5 3] [5 4]
           [6 1] [6 2] [6 3] [6 4] [6 5]]))

  (is (= (unique-tuple 3 6)
          [[3 2 1] 
           [4 2 1] [4 3 1] [4 3 2] 
           [5 2 1] [5 3 1] [5 3 2] [5 4 1] [5 4 2] [5 4 3]
           [6 2 1] [6 3 1] [6 3 2] [6 4 1] [6 4 2] [6 4 3]
           [6 5 1] [6 5 2] [6 5 3] [6 5 4]]))

  (is (= (unique-tuple 4 5)
          [[4 3 2 1] 
           [5 3 2 1] [5 4 2 1] [5 4 3 1][5 4 3 2]])))
              
(run-tests)
              
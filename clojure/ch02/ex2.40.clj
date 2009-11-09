(ns sicp.ch02 (:use clojure.contrib.test-is))

; Using vectors because they work better.

; From ex 1.21
(defn square [n] (* n n))

(defn divides? [test_divisor n] (= (rem n test_divisor) 0))

(defn find_divisor [n test_divisor]
  (cond (> (square test_divisor) n) n
        (divides? test_divisor n) test_divisor
        :else (find_divisor n (+ test_divisor 1))))

(defn smallest_divisor [n] (find_divisor n 2))

(defn prime? [n] (= (smallest_divisor n) n))

; Recall that accumulate is really fold-right
(defn fold-right [op initial sequence]
  (if (empty? sequence)
      initial
      (op (first sequence)
          (fold-right op initial (vec (rest sequence))))))

(defn flatmap [proc seq]
  (vec (fold-right into [] (vec (map proc seq)))))
  
(deftest test-flatmap
  (is (= (flatmap (fn [v] (vec (map #(+ % 1) v))) [[1 2] [3 4 5] [6]])  [2 3 4 5 6 7])))

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
    
(deftest test-enumerate-interval
  (is (= (enumerate-interval 0 0)  [0]))
  (is (= (enumerate-interval 0 5)  [0 1 2 3 4 5]))
  (is (= (enumerate-interval 5 10) [5 6 7 8 9 10])))
  
(deftest test-unique-pairs
  (is (= (unique-pairs 6) 
              [[2 1] 
               [3 1] [3 2]
               [4 1] [4 2] [4 3]
                    [5 1] [5 2] [5 3] [5 4]
                    [6 1] [6 2] [6 3] [6 4] [6 5]])))

(defn pair-sum [pair]
  (+ (first pair) (get pair 1)))

(defn prime-sum? [pair]
  (prime? (pair-sum pair)))
  
(defn make-pair-sum [pair]
  [(first pair) (get pair 1) (pair-sum pair)])
  
(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
       
(deftest test-prime-sum-pairs
  (is (= (prime-sum-pairs 6) 
              [[2 1 3] 
                    [3 2 5] 
                    [4 1 5] [4 3 7]
                    [5 2 7]
                    [6 1 7] [6 5 11]])))

(run-tests)
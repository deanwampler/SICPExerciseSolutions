(ns sicp.ch02 (:use clojure.contrib.test-is))

(defn accumulate [op initial sequence]
  (if (empty? sequence)
      initial
      (op (first sequence)
          (accumulate op initial (rest sequence)))))

; "map" would conflict with library...
(defn map2 [p sequence]
  (accumulate (fn [x y] (cons (p x) y)) (list) sequence))
  
(deftest test-map2
  (is (= (map2 (fn [x] (* x x)) (list)) (list)))
  (is (= (map2 (fn [x] (* x x)) (list 2)) (list 4)))
  (is (= (map2 (fn [x] (* x x)) (list 1 2 3 4 5)) (list 1 4 9 16 25))))

(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))
  
(deftest test-append
  (is (= (append (list) (list)) (list)))
  (is (= (append (list 1) (list)) (list 1)))
  (is (= (append (list) (list 2)) (list 2)))
  (is (= (append (list 1 2 3 4) (list 5 6 7 8)) (list 1 2 3 4 5 6 7 8))))

(defn length [sequence]
  (accumulate (fn [x y] (+ 1 y)) 0 sequence))
  
(deftest test-length
  (is (= (length (list)) 0))
  (is (= (length (list 1)) 1))
  (is (= (length (list 1 2 3 4 5 6 7 8)) 8)))

(run-tests)

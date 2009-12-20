(ns sicp.ch02 (:use clojure.test))

(defn accumulate [op initial sequence]
  (if (empty? sequence)
      initial
      (op (first sequence)
          (accumulate op initial (rest sequence)))))

(defn accumulate-n [op initial sequences]
  (if (empty? (first sequences))
      (list)
      (cons (accumulate   op initial (accumulate (fn [x l] (cons (first x) l)) (list) sequences))
            (accumulate-n op initial (accumulate (fn [x l] (cons (rest x) l)) (list) sequences)))))

(defn dot-product [v1 v2]
  (accumulate + 0 (map * v1 v2)))
  
(defn matrix-*-vector [m v]
  (map (fn [mi] (accumulate + 0 (map * mi v))) m))
  
(defn transpose [m]
  (accumulate-n cons (list) m))
  
(defn matrix-*-matrix [m1 m2]
  (let [cols (transpose m2)]
    (map (fn [row] (map (fn [col] (dot-product row col)) cols)) m1)))
  
; () . () = 0
; (2) . (4) = 8
; (1 2 3) * (4 5 6) = 32
(deftest test-dot-product
  (is (= (dot-product (list) (list)) 0))
  (is (= (dot-product (list 2) (list 4)) 8))
  (is (= (dot-product (list 1 2 3) (list 4 5 6)) 32)))

; (() () ()) * () = (0 0 0)
; ((1) (2) (3)) * (2) = (2 4 6)
; ((1 2 3) (4 5 6) (7 8 9)) * (2 4 6) = (28 64 100)
(deftest test-matrix-*-vector
  (is (= (matrix-*-vector (list (list) (list) (list)) (list))
              (list 0 0 0)))
  (is (= (matrix-*-vector (list (list 1) (list 2) (list 3)) (list 2))
              (list 2 4 6)))
  (is (= (matrix-*-vector (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 2 4 6))
              (list 28 64 100))))

; tr((1) (2) (3)) = ((1 2 3))
; tr((1 2 3) (4 5 6) (7 8 9)) = ((1 4 7) (2 5 8) (3 6 9))

(deftest test-transpose
  (is (= (transpose (list (list 1) (list 2) (list 3)))
              (list (list 1 2 3))))
  (is (= (transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
              (list (list 1 4 7) (list 2 5 8) (list 3 6 9)))))

; ((1)) * ((2)) = ((2))
; ((1 2) (3 4)) * ((5 6) (7 8)) = ((19 22) (43 50))
(deftest test-matrix-*-matrix
  (is (= (matrix-*-matrix (list (list 1)) (list (list 2))) (list (list 2))))
  (is (= (matrix-*-matrix (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8)))
              (list (list 19 22) (list 43 50)))))

(run-tests)

(ns sicp.ch02 (:use clojure.test))

(defn fold-right [op initial sequence]
  (if (empty? sequence)
      initial
      (op (first sequence)
          (fold-right op initial (rest sequence)))))

(defn fold-left [op initial sequence]
  (loop [result initial seq sequence]
    (if (empty? seq)
        result
        (recur (op result (first seq))
              (rest seq)))))

; Clojure doesn't have a list "append" method like scheme has, but we can
; use a vector, since "into" does what we want, then convert back to a list!  
(defn reverse-right [sequence]
  (if (empty? sequence)
      (list)
      (seq (fold-right (fn [x y] (into y [x])) [] sequence))))

; But, the quirky behavior of "into" gives us this easy alternative:
(defn reverse-right2 [sequence] (into (list) sequence))

(defn reverse-left [sequence]
  (fold-left (fn [x y] (cons y x)) (list) sequence))
  
(deftest test-fold-left-right
  (is (= (reverse-right  (list 1 2 3)) (list 3 2 1)))
  (is (= (reverse-right2 (list 1 2 3)) (list 3 2 1)))
  (is (= (reverse-left   (list 1 2 3)) (list 3 2 1)))
  (is (= (reverse-right  (list 1)) (list 1)))
  (is (= (reverse-right  (list 1)) (list 1)))
  (is (= (reverse-left   (list 1)) (list 1)))
  (is (= (reverse-right  (list)) (list)))
  (is (= (reverse-right2 (list)) (list)))
  (is (= (reverse-left   (list)) (list))))

(run-tests)

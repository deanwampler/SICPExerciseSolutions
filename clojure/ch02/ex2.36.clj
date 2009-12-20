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


(deftest test-accumulate-n
  (is (= (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
              (list 22 26 30)))
  (is (= (accumulate-n + 0 (list (list 1) (list 2) (list 3))) (list 6))))

(run-tests)

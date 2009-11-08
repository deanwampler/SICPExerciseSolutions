(ns sicp.ch02 (:use clojure.contrib.test-is))

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



(deftest test-fold-left-right
  (is (= (fold-right / 1 (list 1 2 3)) (/ 3 2)))

  (is (= (fold-left / 1 (list 1 2 3)) (/ 1 6)))

  (is (= (fold-right list (list) (list 1 2 3)) (list 1 (list 2 (list 3 (list))))))

  (is (= (fold-left list (list) (list 1 2 3)) (list (list (list (list) 1) 2) 3))))

(run-tests)

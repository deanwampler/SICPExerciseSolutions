(ns sicp.ch01 (:use clojure.contrib.test-is))

(defn pascals-triangle [row col]
  (cond (= col 1) 1
        (= col row) 1
        :else (+ (pascals-triangle (- row 1) (- col 1)) (pascals-triangle (- row 1) col))))
        
; (for [i (range 5)]
;  (for [j (range i)] (println (str i ", " j ", " (pascals-triangle i j)))))

(deftest test-fib3b
  (is (= 1 (pascals-triangle 1 1)))
  (is (= 1 (pascals-triangle 2 1)))
  (is (= 1 (pascals-triangle 2 2)))
  (is (= 1 (pascals-triangle 3 1)))
  (is (= 2 (pascals-triangle 3 2)))
  (is (= 1 (pascals-triangle 3 3)))
  (is (= 1 (pascals-triangle 4 1)))
  (is (= 3 (pascals-triangle 4 2)))
  (is (= 3 (pascals-triangle 4 3)))
  (is (= 1 (pascals-triangle 4 4)))
  (is (= 1 (pascals-triangle 5 1)))
  (is (= 4 (pascals-triangle 5 2)))
  (is (= 6 (pascals-triangle 5 3)))
  (is (= 4 (pascals-triangle 5 4)))
  (is (= 1 (pascals-triangle 5 5))))

(run-tests)

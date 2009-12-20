(ns sicp.ch02 (:use clojure.test))

(defn accumulate [op initial sequence]
  (if (empty? sequence)
      initial
      (op (first sequence)
          (accumulate op initial (rest sequence)))))

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms] 
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(deftest test-length
  (is (= (horner-eval 2 (list 1 3 0 5 0 1)) 79))
  (is (= (horner-eval 2 (list 2)) 2)))

(run-tests)

(ns sicp.ch01 (:use clojure.test))

(defn apply-double [f]
  (fn [x] (f (f x))))

(defn increment [n] (+ n 1))

(deftest test-apply-double
  (is (= ((apply-double increment) 2) 4))
  (is (= (((apply-double (apply-double apply-double)) increment) 5) 21)))

(run-tests)

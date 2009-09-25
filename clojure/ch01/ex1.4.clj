(ns sicp.ch01 (:use clojure.contrib.test-is))

(defn a-plus-abs-b [a b]
  (+ a (if (> b 0) b (- b))))
  
(deftest test-a-plus-abs-b
  (is (= 3 (a-plus-abs-b 1, -2)))
  (is (= 2 (a-plus-abs-b 1, -1)))
  (is (= 1 (a-plus-abs-b 1, 0)))
  (is (= 2 (a-plus-abs-b 1, 1)))
  (is (= 3 (a-plus-abs-b 1, 2))))

(run-tests)

(ns sicp.ch01 (:use clojure.contrib.test-is))

(defn f [a b c]
  (if (> a b)
        (+ (* a a) (if (> b c) (* b b) (* c c)))
        (+ (* b b) (if (> a c) (* a a) (* c c)))))

(deftest test-f
  (is (= 25 (f 2 3 4)))
  (is (= 25 (f 2 4 3)))
  (is (= 25 (f 4 3 2)))
  (is (= 25 (f 3 4 2)))
  (is (= 25 (f 4 2 3)))
  (is (= 25 (f 3 2 4))))

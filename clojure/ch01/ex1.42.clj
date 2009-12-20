(ns sicp.ch01 (:use clojure.test))

(defn compose [f g]
  (fn [x] (f (g x))))

(defn square [n]    (* n n))
(defn increment [n] (+ n 1))

(deftest test-compose
  (is (= ((compose square increment) 6) 49))
  (is (= ((compose increment square) 6) 37)))

(run-tests)

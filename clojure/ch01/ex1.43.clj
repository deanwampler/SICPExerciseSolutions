(ns sicp.ch01 (:use clojure.contrib.test-is))

(defn compose [f g]
  (fn [x] (f (g x))))

(defn repeated [f n]
  (fn [x] (loop [g f i n]
    (if (= i 1) (g x)
        (recur (compose f g) (- i 1))))))

(defn square [n]    (* n n))

(deftest test-compose
  (is (= ((repeated square 1) 5) 25))
  (is (= ((repeated square 2) 5) 625))
  (is (= ((repeated square 2) 2) 16))
  (is (= ((repeated square 2) 3) 81)))

(run-tests)

(ns sicp.ch02 (:use clojure.test))

(defn make-vect [x y] [x y])
  
(defn xcor-vect [v] (first v))

(defn ycor-vect [v] (nth v 1))
  
(defn make-segment [start-point end-point]  
  [start-point end-point])

(defn start-segment [s] (first s))
  
(defn end-segment [s] (nth s 1))

(def zero-zero (make-vect 0 0))
(def three-two (make-vect 3 2))
(def seg (make-segment zero-zero three-two))

(deftest test-segments
  (is (= (start-segment seg) zero-zero))
  (is (= (end-segment seg) three-two)))

(run-tests)


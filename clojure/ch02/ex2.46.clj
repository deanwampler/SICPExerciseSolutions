(ns sicp.ch02 (:use clojure.contrib.test-is))

(defn make-vect [x y] [x y])
  
(defn xcor-vect [v] (first v))

(defn ycor-vect [v] (nth v 1))
  
(defn add-vect [v1 v2]
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(defn sub-vect [v1 v2]
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(defn scale-vect [v factor]
  (make-vect (* factor (xcor-vect v))
             (* factor (ycor-vect v))))

(def v1 (make-vect 2 3))
(def v2 (make-vect 5 6))

(deftest test-vector
  (is (= (xcor-vect (add-vect v1 v2)) 7))
  (is (= (ycor-vect (add-vect v1 v2)) 9))
  (is (= (xcor-vect (sub-vect v1 v2)) -3))
  (is (= (ycor-vect (sub-vect v1 v2)) -3))
  (is (= (xcor-vect (scale-vect v1 3)) 6))
  (is (= (ycor-vect (scale-vect v1 3)) 9)))
  
(run-tests)

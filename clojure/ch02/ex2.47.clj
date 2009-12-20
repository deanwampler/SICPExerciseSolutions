(ns sicp.ch02 (:use clojure.test))

(defn make-vect [x y] [x y])
  
(defn xcor-vect [v] (first v))

(defn ycor-vect [v] (nth v 1))
  
(defn make-frame1 [origin edge1 edge2]
  [origin edge1 edge2])

(defn origin-frame1 [frame]
  (nth frame 0))

(defn edge1-frame1 [frame]
  (nth frame 1))

(defn edge2-frame1 [frame]
  (nth frame 2))

(defn make-frame2 [origin edge1 edge2]
  [origin [edge1 edge2]])

(defn origin-frame2 [frame]
  (nth frame 0))

(defn edge1-frame2 [frame]
  (first (nth frame 1)))

(defn edge2-frame2 [frame]
  (second (nth frame 1)))

(def o  (make-vect 1 2))
(def e1 (make-vect 2 3))
(def e2 (make-vect 0 4))
(def f1  (make-frame1 o e1 e2))
(def f2  (make-frame2 o e1 e2))

(deftest test-frame
  (is (= (origin-frame1 f1) o))
  (is (= (edge1-frame1 f1) e1))
  (is (= (edge2-frame1 f1) e2))
  (is (= (origin-frame2 f2) o))
  (is (= (edge1-frame2 f2) e1))
  (is (= (edge2-frame2 f2) e2)))
  
(run-tests)

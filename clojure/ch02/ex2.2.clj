(ns sicp.ch02 (:use clojure.contrib.test-is))

(defn make-point [x y] [x y])

(defn x-point [p] (get p 0))
(defn y-point [p] (get p 1))

(defn make-segment [start end] [start end])
  
(defn start-segment [s] (get s 0))
(defn end-segment [s]   (get s 1))

(defn midpoint-segment [s]
  (let [x1 (x-point (start-segment s))
        y1 (y-point (start-segment s))
        x2 (x-point (end-segment s))
        y2 (y-point (end-segment s))]
      (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

(defn print-point [p] (println (format "(%f,%f)" (x-point p) (y-point p))))

(def zero-zero (make-point 0.0 0.0))
(def zero-two  (make-point 0.0 2.0))
(def two-zero  (make-point 2.0 0.0))
(def zero-one  (make-point 0.0 1.0))
(def one-zero  (make-point 1.0 0.0))
(def one-one   (make-point 1.0 1.0))
(def two-two   (make-point 2.0 2.0))
(def m-one-m-one (make-point -1.0 -1.0))
(def m-two-m-two (make-point -2.0 -2.0))

(def zero-zero-zero-zero   (make-segment zero-zero zero-zero))
(def zero-zero-two-two     (make-segment zero-zero two-two))
(def zero-zero-m-one-m-one (make-segment zero-zero m-one-m-one))
(def zero-zero-m-two-m-two (make-segment zero-zero m-two-m-two))
(def zero-zero-two-zero    (make-segment zero-zero two-zero))
(def zero-zero-zero-two    (make-segment zero-zero zero-two))
(def two-two-zero-zero     (make-segment two-two zero-zero))
(def m-two-m-two-zero-zero (make-segment m-two-m-two zero-zero))
(def two-zero-zero-zero    (make-segment two-zero zero-zero))
(def zero-two-zero-zero    (make-segment zero-two zero-zero))
(def m-two-m-two-two-two   (make-segment m-two-m-two two-two))
(def two-two-m-two-m-two   (make-segment two-two m-two-m-two))

(def mid-zero-zero-a (midpoint-segment zero-zero-zero-zero))
(def mid-zero-zero-b (midpoint-segment m-two-m-two-two-two))
(def mid-zero-zero-c (midpoint-segment two-two-m-two-m-two))
(def mid-one-one     (midpoint-segment zero-zero-two-two))
(def mid-one-zero-a  (midpoint-segment zero-zero-two-zero))
(def mid-one-zero-b  (midpoint-segment two-zero-zero-zero))
(def mid-zero-one-a  (midpoint-segment zero-zero-zero-two))
(def mid-zero-one-b  (midpoint-segment zero-two-zero-zero))

(deftest test-segments
  (defn is-equal-point? [p expected-p]
    (is (= (x-point p) (x-point expected-p)))
    (is (= (y-point p) (y-point expected-p))))
  
  (is-equal-point? mid-zero-zero-a zero-zero)
  (is-equal-point? mid-zero-zero-b zero-zero)
  (is-equal-point? mid-zero-zero-c zero-zero)
  (is-equal-point? mid-one-one     one-one)
  (is-equal-point? mid-one-zero-a  one-zero)
  (is-equal-point? mid-one-zero-b  one-zero)
  (is-equal-point? mid-zero-one-a  zero-one)
  (is-equal-point? mid-zero-one-b  zero-one))

(run-tests)

(print-point mid-zero-zero-a)
(print-point mid-zero-zero-b)
(print-point mid-zero-zero-c)
(print-point mid-one-one)
(print-point mid-one-zero-a)
(print-point mid-one-zero-b)
(print-point mid-zero-one-a)
(print-point mid-zero-one-b)
(println "")
(print-point zero-zero)
(print-point one-zero)
(print-point zero-one)
(print-point two-zero)
(print-point zero-two)
(print-point one-one)
(print-point two-two)
(print-point m-one-m-one)
(print-point m-two-m-two)
(println "")

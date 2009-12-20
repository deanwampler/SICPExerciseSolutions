(ns sicp.ch02 (:use clojure.test))
(use '[clojure.contrib.except :only (throw-if)])

(defn make-interval [a b] [a b])
(defn lower-bound [x] (get x 0))
(defn upper-bound [x] (get x 1))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
    
(defn div-interval [x y]
  (mul-interval x (make-interval (/ 1.0 (upper-bound y)) 
                                 (/ 1.0 (lower-bound y)))))
                                 
(def minus-one-one-interval (make-interval -1.0 1.0))
(def zero-one-interval      (make-interval  0.0 1.0))
(def two-three-interval     (make-interval  2.0 3.0))

(def add1 (add-interval minus-one-one-interval zero-one-interval))
(def add2 (add-interval minus-one-one-interval two-three-interval))
(def add3 (add-interval zero-one-interval      two-three-interval))

(def mul1 (mul-interval minus-one-one-interval zero-one-interval))
(def mul2 (mul-interval minus-one-one-interval two-three-interval))
(def mul3 (mul-interval zero-one-interval      two-three-interval))

(def div1 (div-interval zero-one-interval      minus-one-one-interval))
(def div2 (div-interval minus-one-one-interval two-three-interval))
(def div3 (div-interval zero-one-interval      two-three-interval))

(deftest test-interval
  (is (= (lower-bound add1) -1.0))
  (is (= (upper-bound add1)  2.0))
  (is (= (lower-bound add2)  1.0))
  (is (= (upper-bound add2)  4.0))
  (is (= (lower-bound add3)  2.0))
  (is (= (upper-bound add3)  4.0))

  (is (= (lower-bound mul1) -1.0))
  (is (= (upper-bound mul1)  1.0))
  (is (= (lower-bound mul2) -3.0))
  (is (= (upper-bound mul2)  3.0))
  (is (= (lower-bound mul3)  0.0))
  (is (= (upper-bound mul3)  3.0))

  (is (= (lower-bound div1) -1.0))
  (is (= (upper-bound div1)  1.0))
  (is (= (lower-bound div2) -0.5))
  (is (= (upper-bound div2)  0.5))
  (is (= (lower-bound div3)  0.0))
  (is (= (upper-bound div3)  0.5)))
(run-tests)

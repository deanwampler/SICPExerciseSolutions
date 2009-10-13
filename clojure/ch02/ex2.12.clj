(ns sicp.ch02 (:use clojure.contrib.test-is))
(use '[clojure.contrib.except :only (throw-if)])

(defn make-interval [a b] [a b])
(defn lower-bound [x] (get x 0))
(defn upper-bound [x] (get x 1))

(defn make-center-percent [center percent]
  (let [half-delta (/ (* center percent) 100.0)]
    (make-interval (- center half-delta) (+ center half-delta))))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
  
(defn percent [i]
  (let [half-delta (/ (- (upper-bound i) (lower-bound i)) 2.0)]
    (* (/ half-delta (+ (lower-bound i) half-delta)) 100.0)))
  
(def i (make-center-percent 100.0 1.0))

(deftest test-new-mul-interval-with-center-percent 
  (is (= (lower-bound i)  99.0))
  (is (= (upper-bound i) 101.0))
  (is (= (center i)      100.0))
  (is (= (percent i)       1.0)))
  
(run-tests)

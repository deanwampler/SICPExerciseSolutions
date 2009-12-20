(ns sicp.ch02 (:use clojure.test))
(use '[clojure.contrib.except :only (throw-if)])

(defn make-interval [a b] [a b])
(defn lower-bound [x] (get x 0))
(defn upper-bound [x] (get x 1))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (cond (or (= 0 (lower-bound y)) (= 0 (upper-bound y))) (throw (RuntimeException. "div by zero!")))
        :else 
          (mul-interval x (make-interval (/ 1.0 (upper-bound y)) 
                                         (/ 1.0 (lower-bound y)))))
                                 
(try 
  ((div-interval (make-interval 1 2) (make-interval 0 1))
   (throw (Exception. "Should have thrown div/0 exception!")))
  (catch RuntimeException _ ))

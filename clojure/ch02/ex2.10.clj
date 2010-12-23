(ns sicp.ch02 (:use clojure.test))

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
  (cond (or (= 0 (lower-bound y)) (= 0 (upper-bound y))) (throw (ArithmeticException. "div by zero!")))
        :else 
          (mul-interval x (make-interval (/ 1.0 (upper-bound y)) 
                                         (/ 1.0 (lower-bound y)))))
                                 
(deftest test-div-interval
  (is (thrown-with-msg? ArithmeticException #"div by zero!"
    (div-interval (make-interval 1 2) (make-interval 0 1)))))

(run-tests)

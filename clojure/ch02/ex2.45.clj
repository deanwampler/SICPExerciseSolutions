(ns sicp.ch02 (:use clojure.test))

; For testing purposes, we'll just use a vector of an arbitrary element for "painter":
; To distinguish beside from below, use a vector of two elements for beside and a
; vector of 2 vectors for below.
(defn beside [p1 p2] [p1 p2])
(defn below [p1 p2] [[p1] [p2]]) ; treat first as below second
  
(defn right-split-old [painter n]
  (if (= n 0)
      painter
      (let [smaller (right-split-old painter (- n 1))]
        (beside painter (below smaller smaller)))))
        
(defn up-split-old [painter n]
  (if (= n 0)
      painter
      (let [smaller (up-split-old painter (- n 1))]
        (below painter (beside smaller smaller)))))
 
(defn split [f1 f2]
  (defn splt [painter n]
    (if (= n 0)
        painter
        (let [smaller (splt painter (- n 1))]
          (f1 painter (f2 smaller smaller)))))
  (fn [painter n] (splt painter n)))

(defn right-split [painter n]
  ((split beside below) painter n))
(defn up-split [painter n]
  ((split below beside) painter n))
  
(println (right-split 1 1))                     
(deftest test-split-higher-order-function
  (is (= (right-split 1 1) (right-split-old 1 1)))
  (is (= (right-split 1 2) (right-split-old 1 2)))
  (is (= (up-split 1 1)    (up-split-old 1 1)))
  (is (= (up-split 1 2)    (up-split-old 1 2))))   

(run-tests)
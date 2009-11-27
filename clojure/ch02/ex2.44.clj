(ns sicp.ch02 (:use clojure.contrib.test-is))

; For testing purposes, we'll just use a vector of an arbitrary element for "painter":
; To distinguish beside from below, use a vector of two elements for beside and a
; vector of 2 vectors for below.
(defn beside [p1 p2] [p1 p2])
(defn below [p1 p2] [[p1] [p2]]) ; treat first as below second
  
(defn right-split [painter n]
  (if (= n 0)
      painter
      (let [smaller (right-split painter (- n 1))]
        (beside painter (below smaller smaller)))))
        
(defn up-split [painter n]
  (if (= n 0)
      painter
      (let [smaller (up-split painter (- n 1))]
        (below (beside smaller smaller) painter))))
        
(defn corner-split [painter n]
  (if (= n 0)
      painter
      (let [up (up-split painter (- n 1))
            right (right-split painter (- n 1))]
        (let [top-left (beside up up)
              bottom-right (below right right)
              corner (corner-split painter (- n 1))]
          (beside (below painter top-left)
                  (below bottom-right corner))))))
                  
(deftest test-splits
  (is (= (right-split 1 1)  [1 [[1] [1]]]))
  (is (= (up-split 1 1)     [[[1 1]] [1]]))
  (is (= (corner-split 1 1) [[[1] [[1 1]]] [[[[1] [1]]] [1]]])))

(run-tests)
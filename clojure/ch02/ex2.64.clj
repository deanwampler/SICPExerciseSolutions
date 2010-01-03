(ns sicp.ch02 (:use clojure.test))

(defn make-tree [entry left right]
  [entry left right])

(defn partial-tree [elts n]
  (if (= n 0)
      (cons '() elts)
      (let [left-size (quot (- n 1) 2)
            left-result (partial-tree elts left-size)
            left-tree (first left-result)
            non-left-elts (rest left-result)
            right-size (- n (+ left-size 1))
            this-entry (first non-left-elts)
            right-result (partial-tree (rest non-left-elts) right-size)
            right-tree (first right-result)
            remaining-elts (rest right-result)]
        (cons (make-tree this-entry left-tree right-tree) remaining-elts))))
                      
(defn length [v]
  (loop [count 0 v2 v]
    (if (empty? v2)
        count
        (recur (+ count 1) (rest v2)))))

(defn list->tree [elements]
  (first (partial-tree elements (length elements))))
  
(deftest test-vect->tree
  (is (= [5 [1 [] [3 [] []]] [9 [7 [] []] [11 [] []]]] (list->tree [1 3 5 7 9 11]))))
(run-tests)
  
; a. For the tree to be balanced, we want the top "root" element to be the number
; at middle of the list. If there is an odd number N of elements, it will be the
; element at position N/2 (counting from 0). If there is an even number of 
; elements, it will still be position N/2, but this element will actually be 
; the last element in the first half of the list. Actually, the algorithm selects
; that element by dividing the list along the (N-1)/2 position. Then the top 
; element will be the car of the second half of the list.
; The algorithm then recursively forms a tree from each half. For each subtree,
; the middle element becomes the root.
; The algorithm uses the "remaining elements" as a pool from which to draw the
; next elements to process.
; Here is the tree for (1 3 5 7 9 11):
;           5
;       +---+---+
;       1       9
;     +-+-+   +-+-+
;         3   7   11
;
; b. It's O(n), because the algorithm traverses every element once.  

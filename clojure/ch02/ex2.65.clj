(ns sicp.ch02 (:use clojure.test))

(defn entry [tree] (first tree))
(defn left-branch [tree] (nth tree 1))
(defn right-branch [tree] (nth tree 2))

(defn make-tree [entry left right]
  [entry left right])

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (entry set)) true
        (< x (entry set))
          (element-of-set? x (left-branch set))
        (> x (entry set))
          (element-of-set? x (right-branch set))))

(defn adjoin-set [x set]
  (cond (empty? set) (make-tree x [] [])
        (= x (entry set)) set
        (< x (entry set))
          (make-tree (entry set)
                     (adjoin-set x (left-branch set))
                     (right-branch set))
        (> x (entry set))
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set x (right-branch set)))))

; Use tree->vect-2 in Ex. 2.63.
(defn tree->vect [tree]
  (defn copy-to-vect [tree result-vect]
    (if (empty? tree)
        result-vect
        (copy-to-vect (left-branch tree)
                      (cons (entry tree)
                            (copy-to-vect (right-branch tree)
                                          result-vect)))))
  (copy-to-vect tree []))

(defn partial-tree [elts n]
  (if (= n 0)
      (cons [] elts)
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

(defn vect->tree [elements]
  (first (partial-tree elements (length elements))))
  
; One way to implement the functions using what we already have is to convert the
; trees to vects, compute the union/intersection on the vects, and then convert
; back to trees. It's still O(n), although we'll make at least 3 passes through
; the data.

(defn union-set-vects [set1 set2]
  (cond (empty? set1) set2
        (empty? set2) set1
        :else 
          (let [x1 (first set1) x2 (first set2)]
            (cond (= x1 x2)
                    (cons x1 (union-set-vects (rest set1) (rest set2)))
                  (< x1 x2)
                    (cons x1 (union-set-vects (rest set1) set2))
                  :else
                    (cons x2 (union-set-vects set1 (rest set2)))))))

(defn intersection-set-vects [set1 set2]
  (if (or (empty? set1) (empty? set2))
    []
    (let [x1 (first set1) x2 (first set2)]
      (cond (= x1 x2)
              (cons x1 (intersection-set-vects (rest set1) (rest set2)))
            (< x1 x2)
              (intersection-set-vects (rest set1) set2)
            (< x2 x1)
              (intersection-set-vects set1 (rest set2))))))

(defn union-set [set1-tree set2-tree]
  (vect->tree (union-set-vects (tree->vect set1-tree) (tree->vect set2-tree))))
(defn intersection-set [set1-tree set2-tree]
  (vect->tree (intersection-set-vects (tree->vect set1-tree) (tree->vect set2-tree))))
  
;
; Here is the tree for (1 3 5 7 9 11) - from fig. 2.16:
;           5
;       +---+---+
;       3       9
;     +-+-+   +-+-+
;     1       7   11
;
; Here is the tree for (4 5 6 7 8):
;           6
;       +---+---+
;       5       8
;     +-+-+   +-+-+
;     4       7
;
; The resulting trees should have the following elements:
; union: (1 3 4 5 6 7 8 9 11)
;           6
;       +---+---+
;       3       8
;     +-+-+   +-+-+
;     1   4   7   9
;       +-+-+   +-+-+ 
;           5       11
;
; Note that this is equivalent to:
;           6
;       +---+---+
;       4       9
;     +-+-+   +-+-+
;     3   5   8   11
;   +-+-+   +-+-+ 
;   1       7
; However, the algorithm places unmatched items on the right, rather than the left.
; 
; intersection: (5 7)
;         5
;       +-+-+
;           7
; Similarly, the following tree would also be valid, but doesn't result:
;         7
;       +-+-+
;       5

(deftest test-union-set
  (is (= 
    [6 [3 [1 [] []] [4 [] [5 [] []]]]  [8 [7 [] []] [9 [] [11 [] []]]]]
    (union-set 
      [5 [3 [1 [] []] []] [9 [7 [] []] [11 [] []]]]
      [6 [5 [4 [] []] []] [7 [] [8 [] []]]]))))
(deftest test-intersection-set
  (is (= 
    [5 [] [7 [] []]]
    (intersection-set
      [5 [3 [1 [] []] []] [9 [7 [] []] [11 [] []]]]
      [6 [5 [4 [] []] []] [7 [] [8 [] []]]]))))
(run-tests)
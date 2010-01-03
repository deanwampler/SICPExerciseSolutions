(ns sicp.ch02 (:use clojure.test))

; We use vectors, rather than lists, primarily for the convenience of "into"
; for vectors as a substitute for Scheme's "append" on lists.

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

(defn tree->vect-1 [tree]
  (defn tv [t]
    (if (empty? t)
      []
      (into (tv (right-branch t))
            (cons (entry t) (tv (left-branch t))))))
  (reverse (tv tree)))
  
(defn tree->vect-2 [tree]
  (defn copy-to-vect [tree result-vect]
    (if (empty? tree)
        result-vect
        (copy-to-vect (left-branch tree)
                      (cons (entry tree)
                            (copy-to-vect (right-branch tree)
                                          result-vect)))))
  (copy-to-vect tree []))

(def tree1 (make-tree 7 
  (make-tree 3 (make-tree 1 [] []) (make-tree 5 [] [])) 
  (make-tree 9 [] (make-tree 11 [] []))))
  
(def tree2 (make-tree 3 
  (make-tree 1 [] [])
  (make-tree 7 (make-tree 5 [] []) (make-tree 9 [] (make-tree 11 [] [])))))

(def tree3 (make-tree 5
  (make-tree 3 (make-tree 1 [] []) [])
  (make-tree 9 (make-tree 7 [] []) (make-tree 11 [] [])))) 

(deftest test-tree->vect
  (is (= (tree->vect-1 tree1) (tree->vect-2 tree1)))
  (is (= (tree->vect-1 tree2) (tree->vect-2 tree2)))
  (is (= (tree->vect-1 tree3) (tree->vect-2 tree3))))
(run-tests)

; a. Both tree->vect functions traverse the structure depth-first, appending to
; the resulting vect the (left entry right) at each level, where left and right
; will be built up from the lower layers first. Therefore, both functions produce
; the same sorted vect output as the checks demonstrate.
;
; b. Both functions traverse all elements of the tree, an O(n) process, through
; the common structure of the form
;  (f (left-branch) (cons (entry) (f (right-branch) ...)
; where "f" is either tree->vect-1 or copy-to-vect (for tree->vect-2). 
; Hence, these parts of the functions have roughly the same order.
; tree->vect-2 traverses each node once as it cons the element to the result-vect.
; However, tree->vect-1 traverses the nodes roughly O(n) again when it appends to
; the end of vects. So, it grows ~ 2x as fast.
; Here is a rough test (it has overhead from the test itself).

(defn runtime [] (System/currentTimeMillis))

(defn time-tree->vect [n which-run tree->vect-proc tree]
  (print "starting " which-run ": ")
  (let [start-time (runtime)]
    (loop [m n]
      (tree->vect-proc tree)
      (if (= m 0) 
          true
          (recur (- m 1))))
    (println (- (runtime) start-time))))
  
(time-tree->vect 1000 "tree->vect-1 tree1" tree->vect-1 tree1)
(time-tree->vect 1000 "tree->vect-2 tree1" tree->vect-2 tree1)
(time-tree->vect 1000 "tree->vect-1 tree2" tree->vect-1 tree2)
(time-tree->vect 1000 "tree->vect-2 tree2" tree->vect-2 tree2)
(time-tree->vect 1000 "tree->vect-1 tree3" tree->vect-1 tree3)
(time-tree->vect 1000 "tree->vect-2 tree3" tree->vect-2 tree3)
; output:
; starting  tree->vect-1 tree1 : 138
; starting  tree->vect-2 tree1 : 59
; starting  tree->vect-1 tree2 : 135
; starting  tree->vect-2 tree2 : 89
; starting  tree->vect-1 tree3 : 69
; starting  tree->vect-2 tree3 : 100

; So, it looks like other factors dominate for tree3. 
(ns sicp.ch02 (:use clojure.test))

; Set representation with duplicate entries in the list.

; Unchanged, but it will be slower due to the redundant entries.
(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (first set)) true
        :else (element-of-set? x (rest set))))
        
(deftest test-element-of-set?
  (is (= false (element-of-set? 1 '())))
  (is (= false (element-of-set? 1 '(2 3 4))))
  (is (= true  (element-of-set? 1 '(1 2 3 4))))
  (is (= true  (element-of-set? 1 '(1)))))

; Now just cons the element to the set, rather than checking first if x is already
; in the set. This turns the function from O(n) to O(1).
(defn adjoin-set [x set] (cons x set))

(deftest test-adjoin-set
  (is (= '(1) (adjoin-set 1 '())))
  (is (= '(1 1) (adjoin-set 1 '(1))))
  (is (= '(1 2) (adjoin-set 1 '(2))))
  (is (= '(1 1 2) (adjoin-set 1 '(1 2))))
  (is (= '(1 2 3 4) (adjoin-set 1 '(2 3 4)))))

; Must use the same implementation to find the true intersection. Will be slower
; due to the redundant entries, but the resulting set will have no duplicates.
(defn intersection-set [set1 set2]
  (cond (or (empty? set1) (empty? set2)) '()
        (element-of-set? (first set1) set2)
          (cons (first set1)
                (intersection-set (rest set1) set2))
        :else (intersection-set (rest set1) set2)))

(deftest test-intersection-set
  (is (= '() (intersection-set '() '())))
  (is (= '() (intersection-set '() '(1 2 3))))
  (is (= '() (intersection-set '(1 2 3) '())))
  (is (= '(2) (intersection-set '(2) '(2 3 4 5))))
  (is (= '(2) (intersection-set '(2 3 4 5) '(2))))
  (is (= '(2 3) (intersection-set '(2 3 4 5) '(1 2 3))))
  (is (= '(2 3) (intersection-set '(1 2 3) '(2 3 4 5)))))

; Now just joins the two sets, rather than checking first for duplicates.
; This turns the function from O(n) to O(1).
(defn union-set [set1 set2]
  (loop [s1 (reverse set1) result set2]  ; reverse set1 to get the order we want.
    (if (empty? s1) 
      result 
      (recur (rest s1) (cons (first s1) result)))))

(deftest test-union-set
  (is (= '() (union-set '() '())))
  (is (= '(1 2 3) (union-set '() '(1 2 3))))
  (is (= '(1 2 3) (union-set '(1 2 3) '())))
  (is (= '(2 2 3 4 5) (union-set '(2) '(2 3 4 5))))
  (is (= '(2 3 4 5 2) (union-set '(2 3 4 5) '(2))))
  (is (= '(2 3 4 5 1 2 3) (union-set '(2 3 4 5) '(1 2 3))))
  (is (= '(1 2 3 2 3 4 5) (union-set '(1 2 3) '(2 3 4 5)))))

; Since the representations of a set are no longer unique, e.g., 
; '(1 1 1 3) represents the same set as '(1 2), we really need an
; equality method.
(defn equal-sets? [set1 set2]
  (defn check-one [s1 s2]
    (cond (empty? s1) true
          (element-of-set? (first s1) s2)
            (check-one (rest s1) s2)
          :else false))
  (and (check-one set1 set2) (check-one set2 set1)))

(deftest test-equal-sets?
  (is (= true  (equal-sets? '() '())))
  (is (= false (equal-sets? '() '(1))))
  (is (= false (equal-sets? '(1) '())))
  (is (= true  (equal-sets? '(1) '(1))))
  (is (= true  (equal-sets? '(1) '(1 1))))
  (is (= true  (equal-sets? '(1) '(1 1 1))))
  (is (= true  (equal-sets? '(1 1) '(1))))
  (is (= true  (equal-sets? '(1 1 1) '(1))))
  (is (= true  (equal-sets? '(1 2 3 4 5) '(1 2 3 4 5))))
  (is (= true  (equal-sets? '(1 2 3 4 5) '(1 2 1 3 3 2 4 4 4 5 5 4 3 2 1))))
  (is (= true  (equal-sets? '(1 2 1 3 3 2 4 4 4 5 5 4 3 2 1) '(1 2 3 4 5)))))

(run-tests)

; This implementation would be best when adjoin performance is more important 
; than the performance of all the other operations. That is, when you want to 
; add to the set in O(1) time, rather than O(n) time, and you're willing to accept
; performance of the other methods that are still O(n) or O(n*n), but with a higher
; constant value.

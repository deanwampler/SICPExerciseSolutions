(ns sicp.ch02 (:use clojure.test))

; Set representation with ordered entries (assume numbers) in the list.

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (first set)) true
        (< x (first set)) false
        :else (element-of-set? x (rest set))))
          
(deftest test-element-of-set?
  (is (= false (element-of-set? 1 '())))
  (is (= false (element-of-set? 1 '(2 3 4))))
  (is (= true  (element-of-set? 1 '(1 2 3 4))))
  (is (= true  (element-of-set? 1 '(1)))))

(defn append [list1 list2]
  (loop [l1 (reverse list1) l2 list2]
    (if (empty? l1)
        l2
        (recur (rest l1) (cons (first l1) l2)))))

; This is also O(n), but on average takes 1/2 the time of adjoin-set in an
; unordered list implementation, just like element-of-set? above. This is true
; because the traversal of the existing set will stop when one of the two
; conditionals (= or <) is true.
(defn adjoin-set [x set]
  (if (empty? set)
      (list x)
      (let [s (first set)]
        (cond (= x s) set
              (< x s) (cons x set)
              :else (cons s (adjoin-set x (rest set)))))))

(deftest test-adjoin-set
  (is (= '(1) (adjoin-set 1 '())))
  (is (= '(1) (adjoin-set 1 '(1))))
  (is (= '(1 2) (adjoin-set 1 '(2))))
  (is (= '(1 2) (adjoin-set 1 '(1 2))))
  (is (= '(1 2 3 4) (adjoin-set 1 '(1 2 3 4))))
  (is (= '(1 2 3 4) (adjoin-set 2 '(1 2 3 4))))
  (is (= '(1 2 3 4) (adjoin-set 3 '(1 2 3 4))))
  (is (= '(1 2 3 4) (adjoin-set 4 '(1 2 3 4))))
  (is (= '(1 2 3 4) (adjoin-set 1 '(2 3 4))))
  (is (= '(1 2 3 4) (adjoin-set 2 '(1 3 4))))
  (is (= '(1 2 3 4) (adjoin-set 3 '(1 2 4))))
  (is (= '(1 2 3 4) (adjoin-set 4 '(1 2 3)))))

(defn intersection-set [set1 set2]
  (if (or (empty? set1) (empty? set2))
    '()
    (let [x1 (first set1) x2 (first set2)]
      (cond (= x1 x2)
              (cons x1 (intersection-set (rest set1) (rest set2)))
            (< x1 x2)
              (intersection-set (rest set1) set2)
            (< x2 x1)
              (intersection-set set1 (rest set2))))))

(deftest test-intersection-set
  (is (= '() (intersection-set '() '())))
  (is (= '() (intersection-set '() '(1 2 3))))
  (is (= '() (intersection-set '(1 2 3) '())))
  (is (= '(2) (intersection-set '(2) '(2 3 4 5))))
  (is (= '(2) (intersection-set '(2 3 4 5) '(2))))
  (is (= '(2 3) (intersection-set '(2 3 4 5) '(1 2 3))))
  (is (= '(2 3) (intersection-set '(1 2 3) '(2 3 4 5)))))

(run-tests)

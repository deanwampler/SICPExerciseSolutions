(ns sicp.ch02 (:use clojure.test))

; Set representation with no duplicate entries in the list.

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (first set)) true
        :else (element-of-set? x (rest set))))
        
(deftest test-element-of-set?
  (is (= false (element-of-set? 1 '())))
  (is (= false (element-of-set? 1 '(2 3 4))))
  (is (= true  (element-of-set? 1 '(1 2 3 4))))
  (is (= true  (element-of-set? 1 '(1)))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(deftest test-adjoin-set
  (is (= '(1) (adjoin-set 1 '())))
  (is (= '(1 2) (adjoin-set 1 '(2))))
  (is (= '(1 2 3 4) (adjoin-set 1 '(2 3 4)))))

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

(defn union-set [set1 set2]
  (cond (empty? set1) set2
        (empty? set2) set1
        (element-of-set? (first set1) set2)
          (union-set (rest set1) set2)
        :else (cons (first set1)
                (union-set (rest set1) set2))))

(deftest test-union-set
  (is (= '() (union-set '() '())))
  (is (= '(1 2 3) (union-set '() '(1 2 3))))
  (is (= '(1 2 3) (union-set '(1 2 3) '())))
  (is (= '(2 3 4 5) (union-set '(2) '(2 3 4 5))))
  ; union-set doesn't sort.
  (is (= '(3 4 5 2) (union-set '(2 3 4 5) '(2))))
  (is (= '(4 5 1 2 3) (union-set '(2 3 4 5) '(1 2 3))))
  (is (= '(1 2 3 4 5) (union-set '(1 2 3) '(2 3 4 5)))))

(run-tests)
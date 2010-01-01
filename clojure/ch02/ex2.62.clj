(ns sicp.ch02 (:use clojure.test))

; union-set for set representation with ordered entries (assume numbers).

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (first set)) true
        (< x (first set)) false
        :else (element-of-set? x (rest set))))
          
(defn union-set [set1 set2]
  (cond (empty? set1) set2
        (empty? set2) set1
        :else 
          (let [x1 (first set1) x2 (first set2)]
            (cond (= x1 x2)
                    (cons x1 (union-set (rest set1) (rest set2)))
                  (< x1 x2)
                    (cons x1 (union-set (rest set1) set2))
                  :else
                    (cons x2 (union-set set1 (rest set2)))))))

(deftest test-union-set
  (is (= '() (union-set '() '())))
  (is (= '(1 2 3) (union-set '() '(1 2 3))))
  (is (= '(1 2 3) (union-set '(1 2 3) '())))
  (is (= '(2 3 4 5) (union-set '(2) '(2 3 4 5))))
  (is (= '(2 3 4 5) (union-set '(3) '(2 3 4 5))))
  (is (= '(2 3 4 5) (union-set '(4) '(2 3 4 5))))
  (is (= '(2 3 4 5) (union-set '(5) '(2 3 4 5))))
  (is (= '(2 3 4 5) (union-set '(2 3 4 5) '(2))))
  (is (= '(2 3 4 5) (union-set '(2 3 4 5) '(3))))
  (is (= '(2 3 4 5) (union-set '(2 3 4 5) '(4))))
  (is (= '(2 3 4 5) (union-set '(2 3 4 5) '(5))))
  (is (= '(1 2 3 4 5) (union-set '(2 3 4 5) '(1 2 3))))
  (is (= '(1 2 3 4 5) (union-set '(1 2 3) '(2 3 4 5)))))

(run-tests)
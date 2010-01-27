(ns sicp.ch02 (:use clojure.test))

; Use a pair for the key-value.
(defn make-entry [key value] [key value])
(defn entry-key [tree-entry] (first tree-entry))
(defn entry-value [tree-entry] (nth tree-entry 1))

(defn entry [tree] (first tree))
(defn left-branch [tree] (nth tree 1))
(defn right-branch [tree] (nth tree 2))

(defn make-tree [entry left right]
  [entry left right])

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
  
(defn lookup [k set]
  (cond (empty? set) false
        (= k (entry-key (entry set))) (entry-value (entry set))
        (< k (entry-key (entry set))) (lookup k (left-branch set))
        :else (lookup k (right-branch set))))

; Here is the tree for [1 2 3 4 5 6 7 8 9 10], showing the keys only. We'll
; use the 10 * key as the value.
;           5
;       +---+---+
;       3       7
;     +-+-+   +-+-+
;     2   4   6   9
;   +-+-+       +-+-+ 
;   1           8   10

(defn vect-10x->tree []
  (vect->tree
    (loop [n 1 result []]
      (if (> n 10) 
        result
        (recur (+ n 1) (conj result (make-entry n (* n 10))))))))

(deftest test-lookup
  ; Hard-code the tree...
  (is (= 
    (lookup 7 
      '[[5 50] [[3 30] [[2 20] [[1 10] [] []] []] [[4 40] [] []]] 
               [[7 70] [[6 60] [] []] [[9 90] [[8 80] [] []] [[10 100] [] []]]]])
    70))
    (is (= 
    (lookup 11
      '[[5 50] [[3 30] [[2 20] [[1 10] [] []] []] [[4 40] [] []]] 
               [[7 70] [[6 60] [] []] [[9 90] [[8 80] [] []] [[10 100] [] []]]]])
    false))
    ; ... then generate the tree from a list and try that (won't be the same tree...)
    (is (= (lookup 7  (vect-10x->tree)) 70))
    (is (= (lookup 11 (vect-10x->tree)) false)))

(run-tests)
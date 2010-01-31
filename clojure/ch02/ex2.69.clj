(ns sicp.ch02 (:use clojure.test))

(defn make-leaf [symbol weight]
  ['leaf symbol weight])
(defn leaf? [x]
  (= (first x) 'leaf))
(defn symbol-leaf [x] (nth x 1))
(defn weight-leaf [x] (nth x 2))

(defn symbols [tree]
  (if (leaf? tree)
      [(symbol-leaf tree)]
      (nth tree 2)))

(defn weight [tree]
  (if (leaf? tree)
      (weight-leaf tree)
      (nth tree 3)))
      
(defn make-code-tree [left right]
  [left
   right
   (into (symbols left) (symbols right))
   (+ (weight left) (weight right))])
        
(defn left-branch [tree] (first tree))
(defn right-branch [tree] (nth tree 1))

(defn adjoin-set [x set]
  (cond (empty? set) [x]
        (< (weight x) (weight (first set))) (cons x set)
        :else (cons (first set)
                    (adjoin-set x (rest set)))))

(deftest test-adjoin-set
  (is (= (adjoin-set '[leaf C 1] '[[leaf D 1]]) 
          '[[leaf D 1] [leaf C 1]]))
  (is (= (adjoin-set '[leaf B 2] '[[leaf D 1] [leaf C 1]]) 
          '[[leaf D 1] [leaf C 1] [leaf B 2]]))
  (is (= (adjoin-set '[leaf A 4] '[[leaf D 1] [leaf C 1] [leaf B 2]]) 
          '[[leaf D 1] [leaf C 1] [leaf B 2] [leaf A 4]])))
(run-tests)
                    
(defn make-leaf-set [pairs]
  (if (empty? pairs)
      []
      (let [pair (first pairs)]
        (adjoin-set (make-leaf (nth pair 0)   ; symbol
                               (nth pair 1))  ; frequency
                    (make-leaf-set (rest pairs))))))
                    
(deftest test-make-leaf-set
  (is (= (make-leaf-set '[[A 4] [B 2] [C 1] [D 1]]) 
          '[[leaf D 1] [leaf C 1] [leaf B 2] [leaf A 4]])))
(run-tests)

; 'accumulate' and 'length' from Ex. 2.33
(defn accumulate [op initial sequence]
  (if (empty? sequence)
      initial
      (op (first sequence)
          (accumulate op initial (rest sequence)))))
(defn length [sequence]
  (accumulate (fn [x y] (+ 1 y)) 0 sequence))

; Iterate through the set, using adjoin-set to insert at the proper location
; the new subtree that replaces the two lowest-ranked elements in the set (which
; could be either pairs or subtrees).
(defn successive-merge [leaf-set]
  (if (= 1 (length leaf-set)) ; when 1, we have 1 tree, rather than a set of leaves
      (first leaf-set)  ; We have ((set)), so return (set)
      (successive-merge (adjoin-set (make-code-tree (nth leaf-set 0) (nth leaf-set 1)) (rest (rest leaf-set))))))
    
(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))

; Starting with '[[A 4] [B 2] [C 1] [D 1]], we should get the following encoding:
;  
;          + [[A B C D] 8]
;     +----+----+
;     + [A 4]   + [[B C D] 4]
;         +-----+-----+
;         + [B 2]     + [[D C] 2] 
;             +-------+-------+
;             + [D 1]         + [C 1]
;
; Resulting tree:
; [[[leaf A 4] [[leaf B 2] [[leaf D 1] [leaf C 1] [D C] 2] [B D C] 4] [A B D C] 8]]

(deftest test-generate-huffman-tree
  (is (= (generate-huffman-tree '[[C 1] [D 1]])
              '[[leaf D 1] [leaf C 1] [D C] 2]))
              
  (is (= (generate-huffman-tree '[[A 4] [B 2] [C 1] [D 1]])
              '[[leaf A 4] [[leaf B 2] [[leaf D 1] [leaf C 1] [D C] 2] [B D C] 4] [A B D C] 8]))
              
  (is (= (generate-huffman-tree '[[A 8] [B 3] [C 1] [D 1] [E 1] [F 1] [G 1] [H 1]])
              '[[leaf A 8] [[[[leaf H 1] [leaf G 1] [H G] 2] 
                [[leaf F 1] [leaf E 1] [F E] 2] [H G F E] 4] 
                [[[leaf D 1] [leaf C 1] [D C] 2] [leaf B 3] [D C B] 5] 
                  [H G F E D C B] 9] [A H G F E D C B] 17])))
  
(run-tests)


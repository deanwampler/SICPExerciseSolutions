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

(defn choose-branch [bit branch]
    (cond (= bit 0) (left-branch branch)
          (= bit 1) (right-branch branch)
          :else (throw (Exception. "bad bit -- choose-branch" bit))))
          
(defn decode [bits tree]
  (defn decode-1 [bits current-branch]
    (if (empty? bits)
      []
      (let [next-branch (choose-branch (first bits) current-branch)]
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (decode-1 (rest bits) tree))
            (decode-1 (rest bits) next-branch)))))
  (decode-1 bits tree))
  
(defn encode-symbol [symbol tree]
  (defn encode-sym [subtree result]
    (if (leaf? subtree)
        (if (= symbol (symbol-leaf subtree)) result [])
        (let [left-result  (encode-sym (left-branch subtree) (conj result 0))
              right-result (encode-sym (right-branch subtree) (conj result 1))]
          (cond (not (empty? left-result)) left-result
                (not (empty? right-result)) right-result
                :else []))))
  (let [answer (encode-sym tree [])]
    (if (empty? answer)
        (throw (Exception. "Could not encode symbol"))
        answer)))

(defn encode [message tree]
  (if (empty? message)
      []
      (into (encode-symbol (first message) tree)
            (encode (rest message) tree))))

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

(def huffman-tree (generate-huffman-tree '[[a 2] [boom 1] [get 2] [job 2] [na 16] [sha 3] [yip 9] [wah 1]]))
(def message-encoding
  (encode 
    '(get a job 
      sha na na na na na na na na 
      get a job 
      sha na na na na na na na na 
      wah yip yip yip yip yip yip yip yip yip 
      sha boom)
    huffman-tree)) 

(deftest test-encode-variable-size-symbols
  (is (= (encode '[a]    huffman-tree) '[1 1 0 0]))
  (is (= (encode '[boom] huffman-tree) '[1 1 0 1 1]))
  (is (= (encode '[get]  huffman-tree) '[1 1 1 1 1]))
  (is (= (encode '[job]  huffman-tree) '[1 1 1 1 0]))
  (is (= (encode '[na]   huffman-tree) '[0]))
  (is (= (encode '[sha]  huffman-tree) '[1 1 1 0]))
  (is (= (encode '[yip]  huffman-tree) '[1 0]))
  (is (= (encode '[wah]  huffman-tree) '[1 1 0 1 0]))
  (is (= (length message-encoding) 84)))
  
(run-tests)

; So, 84 bits are required to encode this message. 
; With a fixed-length representation, we would need 3 bits (log_2(8)) times the 
; number of symbols, 37, so we would need 3 * 37 = 111.


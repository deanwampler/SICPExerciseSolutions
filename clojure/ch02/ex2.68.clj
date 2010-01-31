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

(def sample-tree
  (make-code-tree 
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree 
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))
        
(def expected-message [0 1 1 0 0 1 0 1 0 1 1 1 0])

(deftest test-make-code-tree
  (is (= (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)) 
         '[[leaf D 1] [leaf C 1] [D C] 2])))
  
(deftest test-encode
  (is (= (encode '[A] sample-tree) [0]))
  (is (= (encode '[B] sample-tree) [1 0]))
  (is (= (encode '[C] sample-tree) [1 1 1]))
  (is (= (encode '[D] sample-tree) [1 1 0]))
  ; (is (= (encode '[E] sample-tree) [1 1 0]))
  (is (= (encode '[A D A B B C A] sample-tree) expected-message)))
  
(run-tests)


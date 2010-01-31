(ns sicp.ch02 (:use clojure.test))

(defn make-leaf [symbol weight]
  ['leaf symbol weight])
(defn leaf? [x]
  (= (first x) 'leaf))
(defn symbol-leaf [x] (nth x 1))
(defn weight-leaf [x] (nth x 2))

(defn symbols [tree]
  (if (leaf? tree)
      [symbol-leaf tree]
      (nth tree 2)))

(defn weight [tree]
  (if (leaf? tree)
      (weight-leaf tree)
      (nth tree 3)))
      
(defn make-code-tree [left right]
  [left
   right
   (conj (symbols left) (symbols right))
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
  
(def sample-tree
  (make-code-tree 
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree 
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))
        
(def sample-message [0 1 1 0 0 1 0 1 0 1 1 1 0])

(deftest test-decode
  (is (= (decode sample-message sample-tree) '[A D A B B C A])))
  
(run-tests)

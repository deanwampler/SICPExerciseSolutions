(ns sicp.ch02 (:use clojure.contrib.test-is))

; I'm going to use vectors for this exercise.
(defn make-mobile [left right] 
  [left right])

(defn make-branch [length structure] 
  [length structure])

(defn left-branch [m]
  (first m))

(defn right-branch [m]
  (get m 1))
  
(defn branch-length [b]
  (first b))

(defn branch-structure [b]
  (get b 1))
  
(defn total-weight [m]
  (defn eval-branch [b]
    (let [struct (branch-structure b)]
      (if (vector? struct)
        (total-weight struct)
        struct)))
  (cond (empty? m) 0
        :else (+ (eval-branch (left-branch m)) (eval-branch (right-branch m)))))
        
(def m1   (make-mobile (make-branch 2 5)  (make-branch 1 10)))
(def m2   (make-mobile (make-branch 3 4)  (make-branch 4 3)))
(def m12  (make-mobile (make-branch 5 m1) (make-branch 6 m2)))
(def m12b (make-mobile (make-branch 7 m1) (make-branch 15 m2)))

(deftest test-mobile-total-weight      
  (is (= (total-weight m1)   15))
  (is (= (total-weight m2)    7))
  (is (= (total-weight m12)  22))
  (is (= (total-weight m12b) 22)))

(defn is-balanced? [m]
  (defn branch-balance [b]
    (let [struct (branch-structure b)
          len (branch-length b)]
      (if (vector? struct)
        (if (is-balanced? struct)
          (* len (total-weight struct))
          false)
        (* len struct))))
  (cond (empty? m) true
        :else (let [l-balance (branch-balance (left-branch m))
                    r-balance (branch-balance (right-branch m))]
                (cond (or (not l-balance) (not r-balance)) false
                      :else (= 0 (- l-balance r-balance))))))

(deftest test-mobile-is-balanced? 
     (is (= (is-balanced? m1)   true))
     (is (= (is-balanced? m2)   true))
     (is (= (is-balanced? m12)  false))
     (is (= (is-balanced? m12b) true)))

(run-tests)
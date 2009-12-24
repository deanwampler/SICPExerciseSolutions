(ns sicp.ch02 (:use clojure.test))

; assume only symbols
(defn equal? [l1 l2]
  (cond (and (empty? l1) (empty? l2)) true
        (or  (empty? l1) (empty? l2)) false
        (=   (first l1) (first l2)) 
          (equal? (rest l1) (rest l2))
        :else false))

(deftest test-equal          
  (is (= true  (equal? '(this is a list) '(this is a list))))
  (is (= false (equal? '(this is a list) '(this (is a) list))))
  (is (= false (equal? '(this is a) '(this is a list))))
  (is (= false (equal? '(this is a list) '(this is a))))
  (is (= false (equal? '(this is A list) '(this is a list))))
  (is (= false (equal? '() '(this is a list))))
  (is (= false (equal? '(this is A list) '())))
  (is (= true  (equal? '() '()))))

(run-tests)
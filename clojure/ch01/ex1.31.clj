(ns sicp.ch01 (:use clojure.test))

(defn prod [term a next b]
  (if (> a b)
    1.0
    (* (term a) (prod term (next a) next b))))

; tail recursive
(defn prod2 [term a next b]
  (loop [a2 a accum 1.0]
    (if (> a2 b)
      accum
      (recur (next a2) (* accum (term a2))))))

(defn ident [n] n)
(defn increment [n] (+ n 1))

(defn fact  [n] (prod  ident 1 increment n))
(defn fact2 [n] (prod2 ident 1 increment n))

(deftest test-fact
  (is (= 1    (fact 1)))
  (is (= 2    (fact 2)))
  (is (= 6    (fact 3)))
  (is (= 24   (fact 4)))  
  (is (= 120  (fact 5)))  
  (is (= 720  (fact 6)))
  (is (= 5040 (fact 7))))

(deftest test-fact2
  (is (= 1    (fact2 1)))
  (is (= 2    (fact2 2)))
  (is (= 6    (fact2 3)))
  (is (= 24   (fact2 4)))  
  (is (= 120  (fact2 5)))  
  (is (= 720  (fact2 6)))
  (is (= 5040 (fact2 7))))

(run-tests)

; fact2 is actually slower! That's believable for low "n", because of the 
; overhead of trampoling, but surprising for large "n".
(time (fact  7))
(time (fact2 7))
(time (fact  200))
(time (fact2 200))


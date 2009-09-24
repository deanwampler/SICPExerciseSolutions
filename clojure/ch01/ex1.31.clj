(ns sicp.ch01 (:use clojure.contrib.test-is))

(defn prod [term a next b]
  (if (> a b)
    1.0
    (* (term a) (prod term (next a) next b))))

; tail recursive
(defn prod2 [term a next b]
  (defn p [a2 accum]
    (if (> a2 b)
      accum
      #(p (next a2) (* accum (term a2)))))
  (trampoline (p a 1.0)))

(defn identity [n] n)
(defn inc [n] (+ n 1))

(defn fact  [n] (prod  identity 1 inc n))
(defn fact2 [n] (prod2 identity 1 inc n))

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

; fact2 is actually slower! That's believable for low "n", because of the 
; overhead of trampoling, but surprising for large "n".
(time (fact  7))
(time (fact2 7))
(time (fact  200))
(time (fact2 200))

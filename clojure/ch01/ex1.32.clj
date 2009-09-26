(ns sicp.ch01 (:use clojure.contrib.test-is))

(defn accumulate [combiner null_value term a next b]
  (if (> a b) null_value 
      (combiner (term a) (accumulate combiner null_value term (next a) next b))))

(defn accumulate2 [combiner null_value term a next b]
  (loop [a2 a accum null_value]
    (if (> a2 b) accum 
      (recur (next a2) (combiner (term a2) accum)))))

(defn prod  [term a next b] (accumulate  (fn [i j] (* i j)) 1 term a next b))
(defn prod2 [term a next b] (accumulate2 (fn [i j] (* i j)) 1 term a next b))

(defn sum  [term a next b] (accumulate  (fn [i j] (+ i j)) 0.0 term a next b))
(defn sum2 [term a next b] (accumulate2 (fn [i j] (+ i j)) 0.0 term a next b))

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

(defn cube [n] (* n n n))
(defn abs [n] (if (> n 0) n (- n)))

(defn simpsons-rule [f a b n]
  (def h (/ (- b a) n))
  (defn fy [k] (f (+ a (* k h))))
  (defn term [i]
    (* (fy i) (if (even? i) 2 4)))
  (* (/ h 3) (+ (fy 0) (sum term 1 increment (- n 1)) (fy n))))

(defn simpsons-rule2 [f a b n]
  (def h (/ (- b a) n))
  (defn fy [k] (f (+ a (* k h))))
  (defn term [i]
    (* (fy i) (if (even? i) 2 4)))
  (* (/ h 3) (+ (fy 0) (sum2 term 1 increment (- n 1)) (fy n))))

(deftest test-simpsons-rule
  (is (> 0.001 (abs (- 0.25 (simpsons-rule cube 0.0 1.0 10)))))
  (is (> 0.001 (abs (- 0.25 (simpsons-rule cube 0.0 1.0 100)))))
  (is (> 0.001 (abs (- 0.25 (simpsons-rule cube 0.0 1.0 1000)))))
  ; Stack overflow
  ; (is (> 0.001 (abs (- 0.25 (simpsons-rule cube 0.0 1.0 10000)))))
  )

(deftest test-simpsons-rule2
  (is (> 0.001 (abs (- 0.25 (simpsons-rule2 cube 0.0 1.0 10)))))
  (is (> 0.001 (abs (- 0.25 (simpsons-rule2 cube 0.0 1.0 100)))))
  (is (> 0.001 (abs (- 0.25 (simpsons-rule2 cube 0.0 1.0 1000)))))
  (is (> 0.001 (abs (- 0.25 (simpsons-rule2 cube 0.0 1.0 10000))))))

(run-tests)


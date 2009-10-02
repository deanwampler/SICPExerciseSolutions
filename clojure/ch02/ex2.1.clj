(ns sicp.ch01 (:use clojure.contrib.test-is))

(defn gcd [a b]
  (if (= b 0)
      a
      (gcd b (rem a b))))
      
(defn abs [n] (Math/abs n))

(defn rationalize-signs [n d]
  (if (< (* n d) 0) 
        [(- (abs n)) (abs d)]
        [(abs n) (abs d)]))

(defn make-rat [n d] 
  (let [nd (rationalize-signs n d)
        g (gcd (abs (first nd)) (abs (first (rest nd))))]
    [(/ (first nd) g) (/ (first (rest nd)) g)]))
    
(defn numer [x] (first x))
(defn denom [x] (first (rest x)))

(defn display-rat [r]
  (println (format "%d/%d" (numer r) (denom r))))
  
(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn check-rat-equal? [r expected-n expected-d]
  (display-rat r)(print ": ")
  (is (= (numer r) expected-n))
  (is (= (denom r) expected-d)))

(def one-half  (make-rat 1 2))
(def one-third (make-rat 1 3))

(def five-sixths   (add-rat one-half  one-third))
(def one-sixth-sub (sub-rat one-half  one-third))
(def one-sixth-mul (mul-rat one-half  one-third))
(def three-halves  (div-rat one-half  one-third))
(def two-thirds    (add-rat one-third one-third))

(def minus-minus-one-half    (make-rat    1     2))
(def minus-one-half          (make-rat (- 1)    2))
(def minus-one-third         (make-rat    1  (- 3)))
(def minus-one-sixth-add-a   (add-rat minus-one-half   one-third))
(def plus-one-sixth-add-b    (add-rat one-half         minus-one-third))
(def minus-five-sixths-sub-a (sub-rat minus-one-half   one-third))
(def plus-five-sixths-sub-b  (sub-rat one-half         minus-one-third))
(def minus-one-sixth-mul-a   (mul-rat minus-one-half   one-third))
(def minus-one-sixth-mul-b   (mul-rat one-half         minus-one-third))
(def minus-three-halves-a    (div-rat minus-one-half   one-third))
(def minus-three-halves-b    (div-rat one-half         minus-one-third))
(def minus-two-thirds-a      (add-rat minus-one-third  minus-one-third))
(def zero-a                  (add-rat minus-one-third  one-third))
(def zero-b                  (add-rat one-third        minus-one-third))

(deftest test-rat
  (is (= (rationalize-signs -1  2) [-1 2]))
  (is (= (rationalize-signs  1 -2) [-1 2]))
  (check-rat-equal? one-half 1 2)
  (check-rat-equal? one-third 1 3)
  (check-rat-equal? five-sixths   5 6)
  (check-rat-equal? one-sixth-sub 1 6)
  (check-rat-equal? one-sixth-mul 1 6)
  (check-rat-equal? three-halves  3 2)
  (check-rat-equal? two-thirds    2 3)

  (is (= (equal-rat? one-half one-third) false))
  (is (= (equal-rat? one-sixth-sub one-sixth-mul) true))

  (check-rat-equal? minus-minus-one-half    1     2)
  (check-rat-equal? minus-one-half       (- 1)    2)
  (check-rat-equal? minus-one-third      (- 1)    3)  

  (check-rat-equal? minus-one-sixth-add-a   (- 1) 6)
  (check-rat-equal? plus-one-sixth-add-b       1  6)
  (check-rat-equal? minus-five-sixths-sub-a (- 5) 6)
  (check-rat-equal? plus-five-sixths-sub-b     5  6)
  (check-rat-equal? minus-one-sixth-mul-a   (- 1) 6)
  (check-rat-equal? minus-one-sixth-mul-b   (- 1) 6)
  (check-rat-equal? minus-three-halves-a    (- 3) 2)
  (check-rat-equal? minus-three-halves-b    (- 3) 2)
  (check-rat-equal? minus-two-thirds-a      (- 2) 3)
  (check-rat-equal? zero-a                     0  1)
  (check-rat-equal? zero-b                     0  1))

(run-tests)

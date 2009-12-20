(ns sicp.ch02 (:use clojure.test))
(use '[clojure.contrib.except :only (throw-if)])

(defn make-interval [a b] [a b])
(defn lower-bound [x] (get x 0))
(defn upper-bound [x] (get x 1))

(defn mul-interval-old [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

; Truth table: 
; - indicates < 0, + indicates >= 0
;   xlow xup ylow yup     (lower-mult,  upper-mult)
; 1  -    -   -    -      (xup * yup,   xlow * ylow)
; 2  -    -   -    +      (xlow * yup,  xlow * ylow)
; 3  -    -   +    +      (xlow * yup,  xup * ylow)
; 4  -    +   -    -      (xup * ylow,   xlow * ylow)
; 5  -    +   -    +      (min(xlow*yup, xup*ylow), max(xlow*ylow,xup*yup))
; 6  -    +   +    +      (xlow * yup,  xup * yup)
; 7  +    +   -    -      (xup * yup,   xup * ylow)
; 8  +    +   -    +      (xup * ylow,  xup * yup)
; 9  +    +   +    +      (xlow * ylow, xup * yup)

(defn mul-interval [x y]
  (defn bounds [lx ux ly uy]
    (and (lx (lower-bound x) 0) (ux (upper-bound x) 0) 
         (ly (lower-bound y) 0) (uy (upper-bound y) 0)))
  (defn mk-interval [a b c d]
    (make-interval (* a b) (* c d))) 

  (cond 
    (bounds < < < <)
      (mk-interval (upper-bound x) (upper-bound y) (lower-bound x) (lower-bound y))
    (bounds < < < >=)
      (mk-interval (lower-bound x) (upper-bound y) (lower-bound x) (lower-bound y))
    (bounds < < >= >=)
      (mk-interval (lower-bound x) (upper-bound y) (upper-bound x) (lower-bound y))
    (bounds < >= < <)
      (mk-interval (upper-bound x) (lower-bound y) (lower-bound x) (lower-bound y))
    (bounds < >= < >=)
      (let [p1 (* (lower-bound x) (upper-bound y))
            p2 (* (upper-bound x) (lower-bound y))
            p3 (* (lower-bound x) (lower-bound y))
            p4 (* (upper-bound x) (upper-bound y))]
        (make-interval (min p1 p1) (max p3 p4))) 
    (bounds < >= >= >=)
      (mk-interval (lower-bound x) (upper-bound y) (upper-bound x) (upper-bound y))
    (bounds >= >= < <)
      (mk-interval (upper-bound x) (upper-bound y) (upper-bound x) (lower-bound y))
    (bounds >= >= < >=)
      (mk-interval (upper-bound x) (lower-bound y) (upper-bound x) (upper-bound y))
    :else   
      (mk-interval (lower-bound x) (lower-bound y) (upper-bound x) (upper-bound y))))

(def minus-three-minus-two-interval (make-interval -3.0 -2.0))
(def minus-two-minus-one-interval (make-interval -2.0 -1.0))
(def minus-two-two-interval (make-interval -2.0 2.0))
(def minus-one-one-interval (make-interval -1.0 1.0))
(def zero-one-interval      (make-interval  0.0 1.0))
(def two-three-interval     (make-interval  2.0 3.0))
(def four-five-interval     (make-interval  4.0 5.0))

(deftest test-new-mul-interval 
  (defn is-equal-mul-intervals? [x y]
    (let [expected (mul-interval-old x y) 
          actual   (mul-interval     x y)]
      (is (= (lower-bound actual) (lower-bound expected)))
      (is (= (upper-bound actual) (upper-bound expected)))))

  ; case 1
  (is-equal-mul-intervals? minus-three-minus-two-interval minus-two-minus-one-interval) 
  ; case 2
  (is-equal-mul-intervals? minus-three-minus-two-interval minus-one-one-interval) 
  ; case 3
  (is-equal-mul-intervals? minus-three-minus-two-interval zero-one-interval) 
  (is-equal-mul-intervals? minus-three-minus-two-interval four-five-interval) 
  ; case 4
  (is-equal-mul-intervals? minus-one-one-interval minus-three-minus-two-interval) 
  ; case 5              
  (is-equal-mul-intervals? minus-two-two-interval minus-one-one-interval) 
  ; case 6              
  (is-equal-mul-intervals? minus-two-two-interval four-five-interval) 
  ; case 7              
  (is-equal-mul-intervals? four-five-interval minus-two-two-interval) 
  ; case 8              
  (is-equal-mul-intervals? four-five-interval minus-two-two-interval) 
  ; case 9              
  (is-equal-mul-intervals? two-three-interval four-five-interval))
(run-tests) 


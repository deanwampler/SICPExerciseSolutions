#lang scheme 
(require (planet schematics/schemeunit:3))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
      
(define (rationalize-signs n d)
  (cond ((< (* n d) 0) (cons (- (abs n)) (abs d)))
        (else (cons (abs n) (abs d)))))
        
(define (make-rat n d) 
  (let ((nd (rationalize-signs n d))
        (g (gcd (abs n) (abs d))))  ; nd is not "visible" here, so can't use it!
    (cons (/ (car nd) g) (/ (cdr nd) g))))
    
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (display-rat r)
  (display (numer r))
  (display "/")
  (display (denom r)))
  
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (check-rat-equal? r expected-n expected-d)
  (display-rat r)(display ": ")
  (check-equal? (numer r) expected-n)
  (check-equal? (denom r) expected-d))

(define one-half  (make-rat 1 2))
(define one-third (make-rat 1 3))

(check-rat-equal? one-half 1 2)
(check-rat-equal? one-third 1 3)

(define five-sixths   (add-rat one-half  one-third))
(define one-sixth-sub (sub-rat one-half  one-third))
(define one-sixth-mul (mul-rat one-half  one-third))
(define three-halves  (div-rat one-half  one-third))
(define two-thirds    (add-rat one-third one-third))

(check-rat-equal? five-sixths   5 6)
(check-rat-equal? one-sixth-sub 1 6)
(check-rat-equal? one-sixth-mul 1 6)
(check-rat-equal? three-halves  3 2)
(check-rat-equal? two-thirds    2 3)

(check-equal? (equal-rat? one-half one-third) false)
(check-equal? (equal-rat? one-sixth-sub one-sixth-mul) true)

(define minus-minus-one-half    (make-rat    1     2))
(define minus-one-half          (make-rat (- 1)    2))
(define minus-one-third         (make-rat    1  (- 3)))
(define minus-one-sixth-add-a   (add-rat minus-one-half   one-third))
(define plus-one-sixth-add-b    (add-rat one-half         minus-one-third))
(define minus-five-sixths-sub-a (sub-rat minus-one-half   one-third))
(define plus-five-sixths-sub-b  (sub-rat one-half         minus-one-third))
(define minus-one-sixth-mul-a   (mul-rat minus-one-half   one-third))
(define minus-one-sixth-mul-b   (mul-rat one-half         minus-one-third))
(define minus-three-halves-a    (div-rat minus-one-half   one-third))
(define minus-three-halves-b    (div-rat one-half         minus-one-third))
(define minus-two-thirds-a      (add-rat minus-one-third  minus-one-third))
(define zero-a                  (add-rat minus-one-third  one-third))
(define zero-b                  (add-rat one-third        minus-one-third))

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
(check-rat-equal? zero-b                     0  1)


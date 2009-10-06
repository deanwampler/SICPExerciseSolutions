#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
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

(define (mul-interval x y)
  (cond ((and (<  (lower-bound x) 0) (<  (upper-bound x) 0) 
              (<  (lower-bound y) 0) (<  (upper-bound y) 0))
                (make-interval (* (upper-bound x) (upper-bound y)) 
                               (* (lower-bound x) (lower-bound y))))
        ((and (<  (lower-bound x) 0) (<  (upper-bound x) 0) 
              (<  (lower-bound y) 0) (>= (upper-bound y) 0))
                (make-interval (* (lower-bound x) (upper-bound y)) 
                               (* (lower-bound x) (lower-bound y))))
        ((and (<  (lower-bound x) 0) (<  (upper-bound x) 0) 
              (>= (lower-bound y) 0) (>= (upper-bound y) 0))
                (make-interval (* (lower-bound x) (upper-bound y)) 
                               (* (upper-bound x) (lower-bound y))))
        ((and (<  (lower-bound x) 0) (>= (upper-bound x) 0) 
              (<  (lower-bound y) 0) (<  (upper-bound y) 0))
                (make-interval (* (upper-bound x) (lower-bound y))
                               (* (lower-bound x) (lower-bound y))))
        ((and (<  (lower-bound x) 0) (>= (upper-bound x) 0) 
              (<  (lower-bound y) 0) (>= (upper-bound y) 0))
                (let ((p1 (* (lower-bound x) (upper-bound y)))
                      (p2 (* (upper-bound x) (lower-bound y)))
                      (p3 (* (lower-bound x) (lower-bound y)))
                      (p4 (* (upper-bound x) (upper-bound y))))
                  (make-interval (min p1 p1) (max p3 p4)))) 
        ((and (<  (lower-bound x) 0) (>= (upper-bound x) 0) 
              (>= (lower-bound y) 0) (>= (upper-bound y) 0))
                (make-interval (* (lower-bound x) (upper-bound y)) 
                               (* (upper-bound x) (upper-bound y))))
        ((and (>= (lower-bound x) 0) (>= (upper-bound x) 0) 
              (<  (lower-bound y) 0) (<  (upper-bound y) 0))
                (make-interval (* (upper-bound x) (upper-bound y)) 
                               (* (upper-bound x) (lower-bound y))))
        ((and (>= (lower-bound x) 0) (>= (upper-bound x) 0) 
              (<  (lower-bound y) 0) (>= (upper-bound y) 0))
                (make-interval (* (upper-bound x) (lower-bound y)) 
                               (* (upper-bound x) (upper-bound y))))
        (else   (make-interval (* (lower-bound x) (lower-bound y)) 
                               (* (upper-bound x) (upper-bound y))))))

(define minus-three-minus-two-interval (make-interval -3.0 -2.0))
(define minus-two-minus-one-interval (make-interval -2.0 -1.0))
(define minus-two-two-interval (make-interval -2.0 2.0))
(define minus-one-one-interval (make-interval -1.0 1.0))
(define zero-one-interval      (make-interval  0.0 1.0))
(define two-three-interval     (make-interval  2.0 3.0))
(define four-five-interval     (make-interval  4.0 5.0))

(define (check-equal-mul-intervals? x y)
  (let ((expected (mul-interval-old x y)) 
        (actual   (mul-interval     x y)))
    (check-equal? (lower-bound actual) (lower-bound expected))
    (check-equal? (upper-bound actual) (upper-bound expected))))
  
; case 1
(check-equal-mul-intervals? minus-three-minus-two-interval minus-two-minus-one-interval) 
; case 2
(check-equal-mul-intervals? minus-three-minus-two-interval minus-one-one-interval) 
; case 3
(check-equal-mul-intervals? minus-three-minus-two-interval zero-one-interval) 
(check-equal-mul-intervals? minus-three-minus-two-interval four-five-interval) 
; case 4
(check-equal-mul-intervals? minus-one-one-interval minus-three-minus-two-interval) 
; case 5              
(check-equal-mul-intervals? minus-two-two-interval minus-one-one-interval) 
; case 6              
(check-equal-mul-intervals? minus-two-two-interval four-five-interval) 
; case 7              
(check-equal-mul-intervals? four-five-interval minus-two-two-interval) 
; case 8              
(check-equal-mul-intervals? four-five-interval minus-two-two-interval) 
; case 9              
(check-equal-mul-intervals? two-three-interval four-five-interval) 


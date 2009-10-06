#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; For subtraction, subtract the upper bound of y from the lower bound of x and
; the lower bound of y from the upper bound of x. The reason is that if the "real"
; value of y is actually towards the upper bounds, then subtracting y from x 
; will yield a lower number than if y is near its lower bound. Hence, we want the
; new lower bound to use the upper bound of y. A similar argument apples to the 
; new upper bound. The highest the new upper bound can be is the value of x's 
; upper bound minus y's lower bound.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
                                 
(define minus-one-one-interval (make-interval -1.0 1.0))
(define zero-one-interval      (make-interval  0.0 1.0))
(define two-three-interval     (make-interval  2.0 3.0))

(define sub1 (sub-interval minus-one-one-interval zero-one-interval))
(define sub2 (sub-interval minus-one-one-interval two-three-interval))
(define sub3 (sub-interval zero-one-interval      two-three-interval))

(check-equal? (lower-bound sub1) -2.0)
(check-equal? (upper-bound sub1)  1.0)
(check-equal? (lower-bound sub2) -4.0)
(check-equal? (upper-bound sub2) -1.0)
(check-equal? (lower-bound sub3) -3.0)
(check-equal? (upper-bound sub3) -1.0)

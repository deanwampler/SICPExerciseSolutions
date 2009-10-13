#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (make-center-percent center percent)
  (let ((half-delta (/ (* center percent) 100.0)))
    (make-interval (- center half-delta) (+ center half-delta))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
  
(define (percent i)
  (let ((half-delta (/ (- (upper-bound i) (lower-bound i)) 2.0)))
    (* (/ half-delta (+ (lower-bound i) half-delta)) 100.0)))
  
(define i (make-center-percent 100.0 1.0))

(check-equal? (lower-bound i)  99.0)
(check-equal? (upper-bound i) 101.0)
(check-equal? (center i)      100.0)
(check-equal? (percent i)       1.0)

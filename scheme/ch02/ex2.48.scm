#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-vect x y) (cons x y))
  
(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))
  
; If start-point and end-point are each (x,y) pairs, then they are already vectors
; as defined by make-vect above. We make a list of the points.
(define (make-segment start-point end-point)  
  (list start-point end-point))

(define (start-segment s) (car s))
  
(define (end-segment s) (cadr s))

(define zero-zero (make-vect 0 0))
(define three-two (make-vect 3 2))
(define seg (make-segment zero-zero three-two))

(check-equal? (start-segment seg) zero-zero)
(check-equal? (end-segment seg) three-two)


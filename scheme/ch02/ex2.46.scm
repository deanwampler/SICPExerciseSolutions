#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-vect x y) (cons x y))
  
(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))
  
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect factor v)
  (make-vect (* factor (xcor-vect v))
             (* factor (ycor-vect v))))

(define v1 (make-vect 2 3))
(define v2 (make-vect 5 6))

(check-equal? (xcor-vect (add-vect v1 v2)) 7)
(check-equal? (ycor-vect (add-vect v1 v2)) 9)
(check-equal? (xcor-vect (sub-vect v1 v2)) -3)
(check-equal? (ycor-vect (sub-vect v1 v2)) -3)
(check-equal? (xcor-vect (scale-vect 3 v1)) 6)
(check-equal? (ycor-vect (scale-vect 3 v1)) 9)

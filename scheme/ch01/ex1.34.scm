#lang scheme 

(define (square x) x * x)

(define (f g) (g 2))

(f square)
(f (lambda (x) (* x (+ x 1))))

(f f)  ; fails to parse
#lang scheme 

(define (square x) x * x)

(define (f g) (g 2))

(f square)
(f (lambda (x) (* x (+ x 1))))

; fails to parse
; (f f)  

#lang scheme 
(require (planet schematics/schemeunit:3))

(define (square x) (* x x))

(define (for-each f items)
  (cond ((null? items))
        (else 
          (f (car items))
          (for-each f (cdr items)))))

(for-each (lambda (x) (display (square x))(newline)) (list 1 2 3 4 5))

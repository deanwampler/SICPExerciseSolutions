#lang scheme
(require (planet schematics/schemeunit:3))

(define (double f)
  (lambda (x) (f (f x))))

(define (inc n) (+ n 1))

(check-equal? ((double inc) 2) 4)

(check-equal? (((double (double double)) inc) 5) 21)
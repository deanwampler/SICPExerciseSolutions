#lang scheme
(require (planet schematics/schemeunit:3))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square n) (* n n))
(define (inc n) (+ n 1))

(check-equal? ((compose square inc) 6) 49)
(check-equal? ((compose inc square) 6) 37)

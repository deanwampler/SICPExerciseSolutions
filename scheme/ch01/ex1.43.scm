#lang scheme
(require (planet schematics/schemeunit:3))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (rep g i)
    (cond ((= i 1) g)
          (else (rep (compose f g) (- i 1)))))
  (lambda (x) ((rep f n) x)))

(define (square n) (* n n))
(define (inc n) (+ n 1))

(check-equal? ((repeated square 1) 5) 25)
(check-equal? ((repeated square 2) 5) 625)
(check-equal? ((repeated square 2) 2) 16)
(check-equal? ((repeated square 2) 3) 81)

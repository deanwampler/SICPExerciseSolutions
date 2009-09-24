#lang scheme 
(require (planet schematics/schemeunit:3))

(define (prod term a next b)
  (define (p a2 accum)
    (if (> a2 b)
      accum
      (p (next a2) (* accum (term a2)))))
  (p a 1))

; tail recursive definition
(define (prod2 term a next b)
  (if (> a b)
    1
    (* (term a) (prod2 term (next a) next b))))

(define (identity n) n)
(define (inc n) (+ n 1))

(define (fact n) (prod identity 1 inc n))
(define (fact2 n) (prod2 identity 1 inc n))

(check-equal? (fact 1) 1)
(check-equal? (fact 2) 2)
(check-equal? (fact 3) 6)
(check-equal? (fact 4) 24)
(check-equal? (fact 5) 120)
(check-equal? (fact 6) 720)
(check-equal? (fact 7) 5040)

(check-equal? (fact2 1) 1)
(check-equal? (fact2 2) 2)
(check-equal? (fact2 3) 6)
(check-equal? (fact2 4) 24)
(check-equal? (fact2 5) 120)
(check-equal? (fact2 6) 720)
(check-equal? (fact2 7) 5040)

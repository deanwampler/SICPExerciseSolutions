#lang scheme 
(require (planet schematics/schemeunit:3))

; Carmichael numbers

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
            m))
        (else 
          (remainder (* base (expmod base (- exp 1) m))
            m))))
          
(define (congruent n)
  (define (c n a)
    (cond ((= a n) true)
          ((= (expmod a n n) (remainder a n)) (c n (+ a 1)))
          (else false)))
  (c n 1))
          
(check-true (congruent 561))
(check-true (congruent 1105))
(check-true (congruent 1729))
(check-true (congruent 2465))
(check-true (congruent 2821))
(check-true (congruent 6601))


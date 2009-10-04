#lang scheme 

(define (cons x y)
  (lambda (m) (m x y)))
  
(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
  
; proof by substitution that:
;  (= x (car z))
(= x (car z))
(= x (z (lambda (p q) p)))                    ; subst. definition of car
(= x ((cons x y) (lambda (p q) p)))           ; subst def of z
(= x ((lambda (m) (m x y)) (lambda (p q) p))) ; subst def of cons
(= x ((m x y) (lambda (p q) p)))              ; subst def of 1st lambda
(= x ((lambda (x y) x)))                      ; subst def of m
(= x x) ; subst def of cons                   ; eval last lambda

; proof by substitution that:
;  (= y (cdr z))
(= y (cdr z))
(= y (z (lambda (p q) q)))                    ; subst. definition of cdr
(= y ((cons x y) (lambda (p q) q)))           ; subst def of z
(= y ((lambda (m) (m x y)) (lambda (p q) q))) ; subst def of cons
(= y ((m x y) (lambda (p q) q)))              ; subst def of 1st lambda
(= y ((lambda (x y) y)))                      ; subst def of m
(= y y) ; subst def of cons                   ; eval last lambda


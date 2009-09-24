#lang scheme 
(require (planet schematics/schemeunit:3))

; Ackermann's Function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
        
(A 1 10) ; 1024
(A 2 4)  ; 65536
(A 3 3)  ; 65536

(define (f n) (A 0 n))  ; => (* 2 n)

(check-equal? (f 0) 0)
(check-equal? (f 1) 2)
(check-equal? (f 2) 4)
(check-equal? (f 3) 6)
(check-equal? (f 4) 8)

(define (g n) (A 1 n))  
; Postulate: 
;  (g n) => (exp2 n) for n > 0
;           0 for n = 0
; where 
(define (exp2 n)
  (cond ((= n 0) 1)
        (else (* 2 (exp2 (- n 1))))))
        
; Proof:
; n = 0 => (g 0) = (A 1 0) = 0
; n = 1 => (g 1) = (A 1 1) = 2
; assume true for n=k
; Proof for n=k+1
;  (g (+ k 1)) = (A 1 (+ k 1))
;              = (A 0 (A 1 k))
;              = (A 0 (exp2 k))
;              = (exp2 (+ k 1))
(check-equal? (g 0) 0)
(check-equal? (g 1) 2)
(check-equal? (g 1) (exp2 1))
(check-equal? (g 2) 4)
(check-equal? (g 2) (exp2 2))
(check-equal? (g 3) 8)
(check-equal? (g 3) (exp2 3))
(check-equal? (g 4) 16)
(check-equal? (g 4) (exp2 4))

(define (h n) (A 2 n))
; for n>2:
; = (A 1 (A 2 (- n 1))) = (A 0 (A 1 (A 2 (- n 2))))
; = (g (A 2 (- n 1))) = (g (A 1 (A 2 (- n 2))))
; = (* 2 (exp2 (A 2 (- n 2))))
; e.g., for n=4: 
; (*2 (exp2 (A 2 (2))))
; (*2 (exp2 (A 1 (A 2 1))))
; (*2 (exp2 (A 1 (2))))
; (*2 (exp2 (exp2 2)))
; (*2 (exp2 4))

; Postulate: 
;  (h n) => (exp2 (h (- n 1)))) for n > 1
;           2 for n = 1
;           0 for n = 0
; where 
; Proof:
; n = 0 => (h 0) = (A 2 0) = 0
; n = 1 => (h 1) = (A 2 1) = 2
; assume true for n=k
; Proof for n=k+1
;  (h (+ k 1)) = (A 2 (+ k 1))
;              = (A 1 (A 2 k))
;              = (exp2 (A 2 k))
;              = (exp2 (h k))
(check-equal? (h 0) 0)
(check-equal? (h 1) 2)
(check-equal? (h 2) 4)
(check-equal? (h 2) (exp2 (h 1)))
(check-equal? (h 3) 16)
(check-equal? (h 3) (exp2 (h 2)))
(check-equal? (h 4) 65536)
(check-equal? (h 4) (exp2 (h 3)))

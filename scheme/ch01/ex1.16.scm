#lang scheme 
(require (planet schematics/schemeunit:3))

(define (square x) (* x x))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
        
(check-equal? (fast-expt 2 1) 2)
(check-equal? (fast-expt 2 2) 4)
(check-equal? (fast-expt 2 3) 8)
(check-equal? (fast-expt 2 4) 16)
(check-equal? (fast-expt 2 5) 32)
(check-equal? (fast-expt 2 6) 64)

; tail recursive form
(define (fast-expt2 b n)
  (define (f a m)
    (cond ((= m 0) a)
          ((even? m) (f (* a (square b)) (- m 2)))
          (else (f (* a b) (- m 1)))))
  (f 1 n))
    
(check-equal? (fast-expt 2 1) 2)
(check-equal? (fast-expt 2 2) 4)
(check-equal? (fast-expt 2 3) 8)
(check-equal? (fast-expt 2 4) 16)
(check-equal? (fast-expt 2 5) 32)
(check-equal? (fast-expt 2 6) 64)

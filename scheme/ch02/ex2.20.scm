#lang scheme 
(require (planet schematics/schemeunit:3))

(define (reverse l)
  (define (rev l result)
    (cond ((null? l) result)
          (else (rev (cdr l) (cons (car l) result)))))
  (rev l '()))

(define (even? x) (= (modulo x 2) 0))
(define (odd? x) (= (modulo x 2) 1))
(define (both op x y) (and (op x) (op y)))

(define (same-parity p . l)
  (define (sm l2 answer)
    (cond ((null? l2) answer)
          ((or (both even? p (car l2)) (both odd? p (car l2))) (sm (cdr l2) (cons (car l2) answer)))
          (else (sm (cdr l2) answer))))
  (reverse (sm l (list p))))

(check-equal? (same-parity 1) (list 1))
(check-equal? (same-parity 2) (list 2))
(check-equal? (same-parity 1 2 3 4 5 6 7) (list 1 3 5 7))
(check-equal? (same-parity 2 3 4 5 6 7) (list 2 4 6))
(check-equal? (same-parity 3 4 5 6 7) (list 3 5 7))
(check-equal? (same-parity 4 5 6 7) (list 4 6))

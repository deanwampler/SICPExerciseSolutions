#lang scheme 
(require (planet schematics/schemeunit:3))

(define (reverse l)
  (define (rev l result)
    (cond ((null? l) result)
          (else (rev (cdr l) (cons (car l) result)))))
  (rev l '()))
              
(check-equal? (reverse (list 1 2 3 4 5 6)) (list 6 5 4 3 2 1))
(check-equal? (reverse (list 1 2)) (list 2 1))
(check-equal? (reverse (list 1)) (list 1))
(check-equal? (reverse '()) '())

#lang scheme 
(require (planet schematics/schemeunit:3))

(define (last-pair l)
  (define (lp l pair)
    (cond ((null? l) pair)
          (else (lp (cdr l) (list (cadr pair) (car l))))))
  (cond ((< (length l) 2) (error "List too short"))
        (else (lp (cddr l) (list (car l) (cadr l))))))
              
(check-equal? (last-pair (list 1 2 3 4 5 6)) (list 5 6))

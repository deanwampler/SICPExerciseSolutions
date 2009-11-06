#lang scheme 
(require (planet schematics/schemeunit:3))

(define (fringe l)
  (define (f l result)
    (cond ((null? l) result)
          ((pair? (car l)) (f (cdr l) (append result (f (car l) '()))))
          (else (f (cdr l) (append result (list (car l)))))))
  (f l '()))

(check-equal? (fringe (list (list 1 2) (list 3 4))) (list 1 2 3 4))
(check-equal? (fringe (list 10 (list 1 2) 20 (list 3 4) 30)) (list 10 1 2 20 3 4 30))
(check-equal? (fringe (list 1 2 3 4 5 6)) (list 1 2 3 4 5 6))
(check-equal? (fringe (list 1 2)) (list 1 2))
(check-equal? (fringe (list 1)) (list 1))
(check-equal? (fringe (list)) (list))

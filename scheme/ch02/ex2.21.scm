#lang scheme 
(require (planet schematics/schemeunit:3))

(define (square-list1 items)
  (if (null? items)
      (list)
      (cons (* (car items) (car items)) (square-list1 (cdr items)))))
      
(check-equal? (square-list1 (list 1 2 3 4 5)) (list 1 4 9 16 25))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(check-equal? (square-list2 (list 1 2 3 4 5)) (list 1 4 9 16 25))


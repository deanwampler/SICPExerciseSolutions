#lang scheme 
(require (planet schematics/schemeunit:3))

(check-equal? (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))) 7)
(check-equal? (car (car (list (list 7)))) 7)
(check-equal? (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr 
  (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))) 7)

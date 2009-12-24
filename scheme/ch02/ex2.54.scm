#lang scheme 
(require (planet schematics/schemeunit:3))

; assume only symbols
(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or  (null? l1) (null? l2)) #f)
        ((eq? (car l1) (car l2)) 
          (equal? (cdr l1) (cdr l2)))
        (else #f)))
          
(check-equal? (equal? '(this is a list) '(this is a list)) #t)
(check-equal? (equal? '(this is a list) '(this (is a) list)) #f)
(check-equal? (equal? '(this is a) '(this is a list)) #f)
(check-equal? (equal? '(this is a list) '(this is a)) #f)
(check-equal? (equal? '(this is A list) '(this is a list)) #f)
(check-equal? (equal? '() '(this is a list)) #f)
(check-equal? (equal? '(this is A list) '()) #f)
(check-equal? (equal? '() '()) #t)

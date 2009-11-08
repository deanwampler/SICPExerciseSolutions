#lang scheme 
(require (planet schematics/schemeunit:3))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial sequences)
  (if (null? (car sequences))
      (list)
      (cons (accumulate   op initial (accumulate (lambda (x l) (cons (car x) l)) (list) sequences))
            (accumulate-n op initial (accumulate (lambda (x l) (cons (cdr x) l)) (list) sequences)))))

(check-equal? (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
              (list 22 26 30))
(check-equal? (accumulate-n + 0 (list (list 1) (list 2) (list 3))) (list 6))             

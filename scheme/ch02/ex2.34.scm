#lang scheme 
(require (planet schematics/schemeunit:3))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
              
(check-equal? (horner-eval 2 (list 1 3 0 5 0 1)) 79)
(check-equal? (horner-eval 2 (list 2)) 2)

#lang scheme 
(require (planet schematics/schemeunit:3))

(define (square n) (* n n))

(define (smallest_divisor n) (find_divisor n 2))

(define (find_divisor n test_divisor)
  (cond ((> (square test_divisor) n) n)
        ((divides? test_divisor n) test_divisor)
        (else (find_divisor n  (+ test_divisor 1)))))

(define (divides? test_divisor n) (= (remainder n test_divisor) 0))

(define (prime? n) (= (smallest_divisor n) n))

(check-equal? (prime? 199) #t)
(check-equal? (prime? 1999) #t)
(check-equal? (prime? 19999) #f)
(check-equal? (smallest_divisor 199) 199)
(check-equal? (smallest_divisor 1999) 1999)
(check-equal? (smallest_divisor 19999) 7)

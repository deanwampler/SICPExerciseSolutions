#lang scheme
(require (planet schematics/schemeunit:3))

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))))
        
(check-equal? (f 0) 0)
(check-equal? (f 1) 1)
(check-equal? (f 2) 2)
(check-equal? (f 3) 3)
(check-equal? (f 4) 6)
(check-equal? (f 5) 11)
(check-equal? (f 6) 20)
(check-equal? (f 7) 37)

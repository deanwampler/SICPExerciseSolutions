#lang scheme
(require (planet schematics/schemeunit:3))

(define (fib3 n)
  (cond ((< n 3) n)
        (else (+ (fib3 (- n 1)) (fib3 (- n 2)) (fib3 (- n 3))))))
        
(check-equal? (fib3 0) 0)
(check-equal? (fib3 1) 1)
(check-equal? (fib3 2) 2)
(check-equal? (fib3 3) 3)
(check-equal? (fib3 4) 6)
(check-equal? (fib3 5) 11)
(check-equal? (fib3 6) 20)
(check-equal? (fib3 7) 37)

(define (fib3b n)
  (define (f i a b c)
    (cond ((< n 3) n)
           ((= i n) (+ a b c))
          (else (f (+ i 1) b c (+ a b c)))))
  (f 3 0 1 2))

(check-equal? (fib3b 0) 0)
(check-equal? (fib3b 1) 1)
(check-equal? (fib3b 2) 2)
(check-equal? (fib3b 3) 3)
(check-equal? (fib3b 4) 6)
(check-equal? (fib3b 5) 11)
(check-equal? (fib3b 6) 20)
(check-equal? (fib3b 7) 37)

  

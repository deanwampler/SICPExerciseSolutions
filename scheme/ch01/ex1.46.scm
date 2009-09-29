#lang scheme 
(require (planet schematics/schemeunit:3))

(define (iterative-improve good-enough? improve)
  (lambda (guess) 
    (define (iterate guess2)
      (if (good-enough? guess2)
        guess2
        (iterate (improve guess2))))
    (iterate guess)))

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))
  
(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (good-enough? guess x))
    (lambda (guess) (improve guess x))) 1.0))
    
(check-= (sqrt 2)    1.414213562373095 0.0001)
(check-= (sqrt 3)    1.732050807568877 0.0001)
(check-= (sqrt 4)    2.0               0.0001)
(check-= (sqrt 4E16) 2E8               1.0)
; For very small X, the original sqrt is horrible; this test fails
; (check-= (sqrt 4E-16) 2E-8 1E-12)

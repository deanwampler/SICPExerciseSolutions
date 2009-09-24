#lang scheme 
(require (planet schematics/schemeunit:3))

; The original implementation in the book:

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))
  
(define (improve guess x)
  (average guess (/ x guess)))
  
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.00001))
  
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
  
; Refined implementation

(define (good-enough2? guess prev-guess x)
  (< (abs (- guess prev-guess)) (* guess 0.00001)))
  
(define (sqrt-iter2 guess prev-guess x)
  (if (good-enough2? guess prev-guess x)
    guess
    (sqrt-iter2 (improve guess x) guess x)))

(define (sqrt2 x)
  (sqrt-iter2 1.0 0.0 x))

; The original sqrt is actually more accurate for large X, if we use a larger
; delta (0.001) in the good-enough? and good-enough2? methods. With 0.00001, 
; they are about the same.
(check-= (sqrt  4E16) 2E8 1.0)
(check-= (sqrt2 4E16) 2E8 1.0)
; For very small X, the original sqrt is horrible; this test fails
(check-= (sqrt  4E-16) 2E-8 0.002) ; note the error range!
(check-= (sqrt2 4E-16) 2E-8 1E-12)

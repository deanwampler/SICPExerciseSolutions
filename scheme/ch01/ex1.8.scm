#lang scheme 
(require (planet schematics/schemeunit:3))

; Newton's Method for Cubes, using the approach in 1.7.

(define (cube x) (* x x x))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  
(define (good-enough? guess prev-guess x)
  (< (abs (- guess prev-guess)) (* guess 0.00001)))
  
(define (cube-root-iter guess prev-guess x)
  (if (good-enough? guess prev-guess x)
    guess
    (cube-root-iter (improve guess x) guess x)))

(define (cube-root x)
  (cube-root-iter 1.0 0.0 x))

(check-= (cube-root 8E15) 2E5 1.0)
(check-= (cube-root 8E-15) 2E-5 1.0)

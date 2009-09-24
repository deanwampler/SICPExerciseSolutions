#lang scheme
(require (planet schematics/schemeunit:3))

(define (cube x) (* x x x))
(define count 0)  ; count invocations of (p x)
(define (p x) 
  (set! count (+ count 1))  ; increment count
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3)))))

(sine 12.15)
(check-equal? count 5)

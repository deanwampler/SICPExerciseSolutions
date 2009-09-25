#lang scheme

; Calculate the golden ratio, phi.

(define tolerance 0.00001)

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))  ; book uses 0.001 for the half-interval discussion.

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))
  
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)  ; 1.6180327868852458


#lang scheme
(require (planet schematics/schemeunit:3))

; Fixed Point, again...

(define tolerance 0.00001)

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))  ; book uses 0.001 for the half-interval discussion.

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (try guess count)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        (begin     ; begin lets us sequence IO and returning "next"
          (display count)
          (display " ")
          next)
        (try next (+ count 1)))))
  (try first-guess 1))
  
(check-= 4.55553 (fixed-point (lambda (x) (/ (log 1000) (log x))) 10.0) tolerance)
; count = 33 
(newline)
(check-= 4.55553 (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 10.0) tolerance)
; count = 10 
(newline)

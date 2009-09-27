#lang scheme
(require (planet schematics/schemeunit:3))

; solving cubic equations

(define dx 0.00001)

(define (close-enough? x y)
  (< (abs (- x y)) dx))
  
(define (fixed-point f first-guess)
  (define (try guess count)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ count 1)))))
  (try first-guess 1))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                dx)))
                
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
  
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
  
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a (* x x)) (* b x) c)))

(newtons-method (cubic 0 0 1) 1)    ; -0.9999999999999863
(newtons-method (cubic 0 1 0) 1)    ; 3.668097353908429e-17
(newtons-method (cubic 1 0 0) 1)    ; 1.1227429100448376e-05
(newtons-method (cubic 1 1 -14) 1)  ; 2.0000000000000133
(newtons-method (cubic 2 1 1) 1)    ; -1.7548776662280976
(newtons-method (cubic 1 2 1) 1)    ; -0.5698402909980529
(newtons-method (cubic 1 1 2) 1)    ; -1.3532099641952162

(check-= (newtons-method (cubic 0 0 1) 1.0) -1.0 dx)
(check-= (newtons-method (cubic 0 1 0) 1.0)  0.0 dx)
; This one probably has significant round-off errors:
(check-= (newtons-method (cubic 1 0 0) 1.0)  0.0 (* 10 dx))
(check-= (newtons-method (cubic 1 1 -14) 1.0) 2.0 dx)

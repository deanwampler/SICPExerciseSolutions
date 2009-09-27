#lang scheme
(require (planet schematics/schemeunit:3))

(define (smooth f dx)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(check-equal? ((smooth (lambda (x) (* 3 x)) 0.5) 2.0) 6.0)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (rep g i)
    (cond ((= i 1) g)
          (else (rep (compose f g) (- i 1)))))
  (lambda (x) ((rep f n) x)))

(define pi-over-4 (/ 3.14159265 4))

(define dx 0.005)
(sin pi-over-4)
((repeated (smooth sin dx) 1) pi-over-4)
((repeated (smooth sin dx) 2) pi-over-4)
((repeated (smooth sin dx) 4) pi-over-4)

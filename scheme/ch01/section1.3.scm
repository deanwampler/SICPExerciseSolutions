#lang scheme

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))
    
(define (inc n) (+ n 1))
(define (cube n) (* n n n))

(define (sum-cubes a b) (sum cube a inc b))

(sum-cubes 2 4)
(sum-cubes 1 10)

(define (identity n) n)

(define (sum-integers a b) (sum identity a inc b))

(sum-integers 2 4)
(sum-integers 1 10)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
    dx))
    
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
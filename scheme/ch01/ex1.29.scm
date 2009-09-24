#lang scheme 
(require (planet schematics/schemeunit:3))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))
    
(define (inc n) (+ n 1))
(define (even? n) (= 0 (remainder n 2)))

(define (simpsons-rule f a b n)
  (define (h) (/ (- b a) n))
  (define (rule h)
    (define (fy k h) (f (+ a (* k h))))
    (define (term i)
      (* (fy i h) (if (even? i) 2 4)))
    (* (/ h 3) (+ (fy 0 h) (sum term 1 inc (- n 1)) (fy n h))))
  (rule (h)))
  
(define (cube n) (* n n n))

(check-= (simpsons-rule cube 0.0 1.0 10)    0.25 0.001)
(check-= (simpsons-rule cube 0.0 1.0 100)   0.25 0.001)
(check-= (simpsons-rule cube 0.0 1.0 1000)  0.25 0.001)
(check-= (simpsons-rule cube 0.0 1.0 10000) 0.25 0.001)

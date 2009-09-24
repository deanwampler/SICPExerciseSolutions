#lang scheme 
(require (planet schematics/schemeunit:3))

(define (accumulate combiner null_value term a next b)
  (if (> a b) null_value 
      (combiner (term a) (accumulate combiner null_value term (next a) next b))))

(define (accumulate2 combiner null_value term a next b)
  (define (ac a2 accum)
    (if (> a2 b) accum 
      (ac (next a2) (combiner (term a2) accum))))
  (ac a null_value))

(define (prod  term a next b) (accumulate  (lambda (i j) (* i j)) 1 term a next b))
(define (prod2 term a next b) (accumulate2 (lambda (i j) (* i j)) 1 term a next b))

(define (sum  term a next b) (accumulate  (lambda (i j) (+ i j)) 0.0 term a next b))
(define (sum2 term a next b) (accumulate2 (lambda (i j) (+ i j)) 0.0 term a next b))

(define (identity n) n)
(define (inc n) (+ n 1))

(define (fact  n) (prod  identity 1 inc n))
(define (fact2 n) (prod2 identity 1 inc n))

(check-equal? (fact 1) 1)
(check-equal? (fact 2) 2)
(check-equal? (fact 3) 6)
(check-equal? (fact 4) 24)  
(check-equal? (fact 5) 120)  
(check-equal? (fact 6) 720)
(check-equal? (fact 7) 5040)

(check-equal? (fact2 1) 1)
(check-equal? (fact2 2) 2)
(check-equal? (fact2 3) 6)
(check-equal? (fact2 4) 24)  
(check-equal? (fact2 5) 120)  
(check-equal? (fact2 6) 720)
(check-equal? (fact2 7) 5040)

(define (cube n) (* n n n))
(define (even? n) (= 0 (remainder n 2)))
(define (abs n) (if (> n 0) n (- n)))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (fy k) (f (+ a (* k h))))
  (define (term i)
    (* (fy i) (if (even? i) 2 4)))
  (* (/ h 3) (+ (fy 0) (sum term 1 inc (- n 1)) (fy n))))

(define (simpsons-rule2 f a b n)
  (define h (/ (- b a) n))
  (define (fy k) (f (+ a (* k h))))
  (define (term i)
    (* (fy i) (if (even? i) 2 4)))
  (* (/ h 3) (+ (fy 0) (sum2 term 1 inc (- n 1)) (fy n))))

(check-= (simpsons-rule cube 0.0 1.0 10)    0.25 0.001)
(check-= (simpsons-rule cube 0.0 1.0 100)   0.25 0.001)
(check-= (simpsons-rule cube 0.0 1.0 1000)  0.25 0.001)
(check-= (simpsons-rule cube 0.0 1.0 10000) 0.25 0.001)

(check-= (simpsons-rule2 cube 0.0 1.0 10)    0.25 0.001)
(check-= (simpsons-rule2 cube 0.0 1.0 100)   0.25 0.001)
(check-= (simpsons-rule2 cube 0.0 1.0 1000)  0.25 0.001)
(check-= (simpsons-rule2 cube 0.0 1.0 10000) 0.25 0.001)


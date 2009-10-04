#lang scheme 
(require (planet schematics/schemeunit:3))

(define (even? n) (= 0 (remainder n 2)))
(define (square x) (* x x))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; To find a, keep dividing z = (cons a b) by 2, counting the number of times.
(define (find-a z)
  (define (f a left-over)
    (cond ((= (remainder left-over 2) 0) (f (+ a 1) (/ left-over 2)))
          (else a)))
  (f 0 z))

; To find b, keep dividing z = (cons a b) by 3, counting the number of times.
(define (find-b z)
  (define (f b left-over)
    (cond ((= (remainder left-over 3) 0) (f (+ b 1) (/ left-over 3)))
          (else b)))
  (f 0 z))

(define (cons a b)
  (* (fast-expt 2 a) (fast-expt 3 b)))

(define (car z) (find-a z))
(define (cdr z) (find-b z))

(define zero-zero (cons 0 0))
(check-equal? (car zero-zero) 0)
(check-equal? (cdr zero-zero) 0)

(define one-two (cons 1 2))
(check-equal? (car one-two) 1)
(check-equal? (cdr one-two) 2)

(define two-one (cons 2 1))
(check-equal? (car two-one) 2)
(check-equal? (cdr two-one) 1)

(define four-five (cons 4 5))
(check-equal? (car four-five) 4)
(check-equal? (cdr four-five) 5)


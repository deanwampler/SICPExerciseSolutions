#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
    
(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y)) 
                                 (/ 1.0 (lower-bound y)))))
                                 
(define minus-one-one-interval (make-interval -1.0 1.0))
(define zero-one-interval      (make-interval  0.0 1.0))
(define two-three-interval     (make-interval  2.0 3.0))

(define add1 (add-interval minus-one-one-interval zero-one-interval))
(define add2 (add-interval minus-one-one-interval two-three-interval))
(define add3 (add-interval zero-one-interval      two-three-interval))

(check-equal? (lower-bound add1) -1.0)
(check-equal? (upper-bound add1)  2.0)
(check-equal? (lower-bound add2)  1.0)
(check-equal? (upper-bound add2)  4.0)
(check-equal? (lower-bound add3)  2.0)
(check-equal? (upper-bound add3)  4.0)

(define mul1 (mul-interval minus-one-one-interval zero-one-interval))
(define mul2 (mul-interval minus-one-one-interval two-three-interval))
(define mul3 (mul-interval zero-one-interval      two-three-interval))

(check-equal? (lower-bound mul1) -1.0)
(check-equal? (upper-bound mul1)  1.0)
(check-equal? (lower-bound mul2) -3.0)
(check-equal? (upper-bound mul2)  3.0)
(check-equal? (lower-bound mul3)  0.0)
(check-equal? (upper-bound mul3)  3.0)

(define div1 (div-interval zero-one-interval      minus-one-one-interval))
(define div2 (div-interval minus-one-one-interval two-three-interval))
(define div3 (div-interval zero-one-interval      two-three-interval))

(check-equal? (lower-bound div1) -1.0)
(check-equal? (upper-bound div1)  1.0)
(check-equal? (lower-bound div2) -0.5)
(check-equal? (upper-bound div2)  0.5)
(check-equal? (lower-bound div3)  0.0)
(check-equal? (upper-bound div3)  0.5)

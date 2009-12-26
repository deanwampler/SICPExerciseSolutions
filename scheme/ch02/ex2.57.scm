#lang scheme 
(require (planet schematics/schemeunit:3))

(define (=number? exp num)
  (and (number? exp) (= exp num)))
  
(define (variable? e) (symbol? e))
  
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
  
(define (addend e) (cadr e))
  
(define (rest-of-operands e op)
  (define (rest exp total)
    (cond ((null? exp) total)
          (else (rest (cdr exp) (op total (car exp))))))
  (rest (cdddr e) (caddr e)))
  
(define (augend e) (rest-of-operands e make-sum))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
  
(define (difference? e)
  (and (pair? e) (eq? (car e) '-)))

(define (minuend e) (cadr e))
  
; Since all the subtrahend expressions are subtraced from the minuend, we use
; make-sum to compute them. i.e., (- x1 x2 x3 ...) => (- x1 (+ x2 x3 ...))
(define (subtrahend e) (rest-of-operands e make-sum))
  
(define (make-difference a1 a2) 
  (cond ((and (number? a1) (number? a2)) (- a1 a2))
        ((=number? a1 0) (list '- 0 a2))
        ((=number? a2 0) a1)
        (else (list '- a1 a2))))
  
(define (product? e)
  (and (pair? e) (eq? (car e) '*)))
  
(define (multiplier e) (cadr e))
  
(define (multiplicand e) (rest-of-operands e make-product))
  
(define (make-product a1 a2) 
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))
  
(define (base e) (cadr e))
  
; We multiple the exponents
(define (exponent e) (rest-of-operands e make-product))
  
(define (make-exponentiation a1 a2) 
  (cond ((=number? a1 0) 0)
        ((=number? a2 0) 1)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 (make-exponentiation a1 (- a2 1))))
        (else (list '** a1 a2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
        ((difference? exp)
          (make-difference (deriv (minuend exp) var)
                           (deriv (subtrahend exp) var)))
        ((product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        ((exponentiation? exp)
          (make-product
            (make-product (exponent exp)
                          (make-exponentiation (base exp) (make-difference (exponent exp) 1)))
            (make-product (deriv (base exp) var)
                          1)))
        (else 
          (error "unknown expression type -- DERIV" exp))))

(check-equal?  (deriv '(+ x 3) 'x) 1)
(check-equal?  (deriv '(+ x 3 4) 'x) 1)
(check-equal?  (deriv '(+ x x 3) 'x) 2)
(check-equal?  (deriv '(- x 3) 'x) 1)
(check-equal?  (deriv '(- x x 3) 'x) 0)
(check-equal?  (deriv '(- x x x x 3) 'x) -2)
(check-equal?  (deriv '(* x y) 'x) 'y)
(check-equal?  (deriv '(* x x y) 'x) '(+ (* x y) (* x y))) ; = '(* 2 x y)
(check-equal?  (deriv '(* (* x y) (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))
(check-equal?  (deriv '(* x y (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))
(check-equal?  (deriv '(* (* x y) (- x 3)) 'x) '(+ (* x y) (* y (- x 3))))
(check-equal?  (deriv '(* x y (- x 3)) 'x) '(+ (* x y) (* y (- x 3))))

(check-equal?  (deriv '(** x 1) 'x) 1)
(check-equal?  (deriv '(** x 2) 'x) '(* 2 x))
(check-equal?  (deriv '(** x 3) 'x) '(* 3 (** x 2)))
(check-equal?  (deriv '(** x 2 3) 'x) '(* 6 (** x 5)))
(check-equal?  (deriv '(** x 4) 'x) '(* 4 (** x 3)))
(check-equal?  (deriv '(** x n) 'x) '(* n (** x (- n 1))))

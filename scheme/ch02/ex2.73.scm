#lang scheme 
(require (planet schematics/schemeunit:3))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
          
; Adapted from Ex. 2.57. One key change. Before, the first operand was "cadr" of
; the expression and the operator was the "car". Now the operator is removed 
; before calling "addend", etc., so the first operand is "car" and the rest of
; them are "cdr".
(define (=number? exp num)
  (and (number? exp) (= exp num)))
  
(define (variable? e) (symbol? e))
  
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
(define (addend e) (car e))
  
(define (rest-of-operands e op)
  (define (rest exp total)
    (cond ((null? exp) total)
          (else (rest (cdr exp) (op total (car exp))))))
  (rest (cddr e) (cadr e)))
  
(define (augend e) (rest-of-operands e make-sum))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
  
(define (minuend e) (car e))
  
; Since all the subtrahend expressions are subtraced from the minuend, we use
; make-sum to compute them. i.e., (- x1 x2 x3 ...) => (- x1 (+ x2 x3 ...))
(define (subtrahend e) (rest-of-operands e make-sum))
  
(define (make-difference a1 a2) 
  (cond ((and (number? a1) (number? a2)) (- a1 a2))
        ((=number? a1 0) (list '- 0 a2))
        ((=number? a2 0) a1)
        (else (list '- a1 a2))))
  
(define (multiplier e) (car e))
  
(define (multiplicand e) (rest-of-operands e make-product))
  
(define (make-product a1 a2) 
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))

(define (base e) (car e))
  
; We multiple the exponents
(define (exponent e) (rest-of-operands e make-product))
  
(define (make-exponentiation a1 a2) 
  (cond ((=number? a1 0) 0)
        ((=number? a2 0) 1)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 (make-exponentiation a1 (- a2 1))))
        (else (list '** a1 a2))))

; a. All the operators have uniform behavior; they are applied to a list of
; operands, which are numbers, variables, or arithmetic combinations of them
; with operators. Numbers and variables are "leaf nodes" in expression trees,
; requiring unique handling.
;
; b. and c. procedures for sums, differences, products, and exponentials.
; Since we don't yet have "get" and "put" (until 3.3.3), we'll use a hack
; to do what we want. We'll ignore the table key 'deriv and just worry about
; the operator.
(define table '())
(define (get ignore op)
  (define (find-op list)
    (cond ((null? list) (error "No match found for " op))
          ((eq? op (car (car list))) (cadr (car list)))
          (else (find-op (cdr list)))))
  (find-op table))
  
(define (put key table-key proc)
  (set! table (cons (list key proc) table)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
      
(define (install-deriv-package)
  (define (sum operands var) 
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (define (difference operands var) 
    (make-difference (deriv (minuend operands) var)
                     (deriv (subtrahend operands) var)))
  (define (product operands var) 
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))))
  (define (exponentiation operands var)
    (make-product
      (make-product (exponent operands)
                    (make-exponentiation (base operands) 
                                         (make-difference (exponent operands) 1)))
      (make-product (deriv (base operands) var)
                    1)))

  (define (tag x) (attach-tag 'deriv x))
  (put '+  '(deriv) sum)
  (put '-  '(deriv) difference)
  (put '*  '(deriv) product)
  (put '** '(deriv) exponentiation)
  'done)
(install-deriv-package)

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

; d. If the indexing of get changed, only the way we implement get and put
; would change; we could simply put the arguments to them in the opposite order
; but still use them in the same way. However, we could also choose to invert the
; the table storage and put the "row emphasis" on the operation and have different
; implementations depending on whether we are doing derivatives or something else.
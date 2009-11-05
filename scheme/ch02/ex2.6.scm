#lang scheme 

; Church Numerals

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
  

(define one (add-1 zero))
; (define one (lambda (f) (lambda (x) (f ((zero f) x)))))
; (define one (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x)))))
; (define one (lambda (f) (lambda (x) (f ((lambda (y) y) x)))))
; (define one (lambda (f) (lambda (x) (f x))))


(define two (add-1 one))
; (define two (lambda (f) (lambda (x) (f ((one f) x)))))
; (define two (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) (g y))) f) x)))))
; (define two (lambda (f) (lambda (x) (f (((lambda (y) (f y))) x)))))
; (define two (lambda (f) (lambda (x) (f (f x)))))

; general case (n>=2)
; (define n (lambda (f) (lambda (x) (f ... (f x) ... ))))

(define (church-numeral n)
  (lambda (f) 
    (lambda (x) 
      (define (g m)
        (cond ((= m 0) x)
              (else (f (g (- m 1))))))
      (g n))))

; (define (+ m n) (church-numeral (+ m n)))

(display zero)(newline)
(display one)(newline)
(display two)(newline)
(display (church-numeral 3))(newline)
(display (church-numeral 4))(newline)

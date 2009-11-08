#lang scheme 
(require (planet schematics/schemeunit:3))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))
  
(check-equal? (map (lambda (x) (* x x)) (list)) (list))
(check-equal? (map (lambda (x) (* x x)) (list 2)) (list 4))
(check-equal? (map (lambda (x) (* x x)) (list 1 2 3 4 5)) (list 1 4 9 16 25))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
  
(check-equal? (append (list) (list)) (list))
(check-equal? (append (list 1) (list)) (list 1))
(check-equal? (append (list) (list 2)) (list 2))
(check-equal? (append (list 1 2 3 4) (list 5 6 7 8)) (list 1 2 3 4 5 6 7 8))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
  
(check-equal? (length (list)) 0)
(check-equal? (length (list 1)) 1)
(check-equal? (length (list 1 2 3 4 5 6 7 8)) 8)


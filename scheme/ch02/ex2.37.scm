#lang scheme 
(require (planet schematics/schemeunit:3))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial sequences)
  (if (null? (car sequences))
      (list)
      (cons (accumulate   op initial (accumulate (lambda (x l) (cons (car x) l)) (list) sequences))
            (accumulate-n op initial (accumulate (lambda (x l) (cons (cdr x) l)) (list) sequences)))))

(define (dot-product v1 v2)
  (accumulate + 0 (map * v1 v2)))
  
(define (matrix-*-vector m v)
  (map (lambda (mi) (accumulate + 0 (map * mi v))) m))
  
(define (transpose m)
  (accumulate-n cons (list) m))
  
(define (matrix-*-matrix m1 m2)
  (let ((cols (transpose m2)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m1)))
  
; () . () = 0
; (2) . (4) = 8
; (1 2 3) * (4 5 6) = 32
(check-equal? (dot-product (list) (list)) 0)
(check-equal? (dot-product (list 2) (list 4)) 8)
(check-equal? (dot-product (list 1 2 3) (list 4 5 6)) 32)

; (() () ()) * () = (0 0 0)
; ((1) (2) (3)) * (2) = (2 4 6)
; ((1 2 3) (4 5 6) (7 8 9)) * (2 4 6) = (28 64 100)
(check-equal? (matrix-*-vector (list (list) (list) (list)) (list))
              (list 0 0 0))
(check-equal? (matrix-*-vector (list (list 1) (list 2) (list 3)) (list 2))
              (list 2 4 6))
(check-equal? (matrix-*-vector (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 2 4 6))
              (list 28 64 100))

; tr((1) (2) (3)) = ((1 2 3))
; tr((1 2 3) (4 5 6) (7 8 9)) = ((1 4 7) (2 5 8) (3 6 9))

(check-equal? (transpose (list (list 1) (list 2) (list 3)))
              (list (list 1 2 3)))
(check-equal? (transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
              (list (list 1 4 7) (list 2 5 8) (list 3 6 9)))

; ((1)) * ((2)) = ((2))
; ((1 2) (3 4)) * ((5 6) (7 8)) = ((19 22) (43 50))
(check-equal? (matrix-*-matrix (list (list 1)) (list (list 2))) (list (list 2)))
(check-equal? (matrix-*-matrix (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8)))
              (list (list 19 22) (list 43 50)))


#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-vect x y) (cons x y))
  
(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))
  
(define (origin-frame1 frame)
  (car frame))

(define (edge1-frame1 frame)
  (cadr frame))

(define (edge2-frame1 frame)
  (caddr frame))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (cadr frame))

(define (edge2-frame2 frame)
  (cddr frame))

(define o  (make-vect 1 2))
(define e1 (make-vect 2 3))
(define e2 (make-vect 0 4))
(define f1  (make-frame1 o e1 e2))
(define f2  (make-frame2 o e1 e2))

(check-equal? (origin-frame1 f1) o)
(check-equal? (edge1-frame1 f1) e1)
(check-equal? (edge2-frame1 f1) e2)
(check-equal? (origin-frame2 f2) o)
(check-equal? (edge1-frame2 f2) e1)
(check-equal? (edge2-frame2 f2) e2)


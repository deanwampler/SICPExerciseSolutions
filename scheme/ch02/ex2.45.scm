#lang scheme 
(require (planet schematics/schemeunit:3))

; For testing purposes, we'll just use a list of an arbitrary element for "painter":
; To distinguish beside from below, use a list of two elements for beside and a
; list of 2 lists for below.
(define (beside p1 p2)
  (list p1 p2))
(define (below p1 p2)
  (list (list p1) (list p2)))  ; treat first as below second
  
(define (right-split-old painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split-old painter (- n 1))))
        (beside painter (below smaller smaller)))))
        
(define (up-split-old painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split-old painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (split f1 f2)
  (define (splt painter n)
    (if (= n 0)
        painter
        (let ((smaller (splt painter (- n 1))))
          (f1 painter (f2 smaller smaller)))))
  (lambda (painter n) (splt painter n)))

(define right-split (split beside below))
(define up-split    (split below beside))

(check-equal? (right-split 1 1)  (right-split-old 1 1))
(check-equal? (right-split 1 2)  (right-split-old 1 2))
(check-equal? (up-split 1 1)     (up-split-old 1 1))
(check-equal? (up-split 1 2)     (up-split-old 1 2))

#lang scheme 
(require (planet schematics/schemeunit:3))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
        
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
        
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
                  
; For testing purposes, we'll just use a list of an arbitrary element for "painter":
; To distinguish beside from below, use a list of two elements for beside and a
; list of 2 lists for below.
(define (beside p1 p2)
  (list p1 p2))
(define (below p1 p2)
  (list (list p1) (list p2)))  ; treat first as below second
  
(check-equal? (right-split 1 1)  (list 1 (list (list 1) (list 1))))
(check-equal? (up-split 1 1)     (list (list 1) (list (list 1 1))))
(check-equal? (corner-split 1 1) (list (list (list 1) (list (list 1 1))) (list (list (list (list 1) (list 1))) (list 1))))

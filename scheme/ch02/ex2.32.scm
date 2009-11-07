#lang scheme 
(require (planet schematics/schemeunit:3))

(define (subsets s)
  (if (null? s) 
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(check-equal? (subsets (list 1 2 3)) 
              (list (list) (list 3) (list 2) (list 2 3) (list 1) (list 1 3) (list 1 2) (list 1 2 3)))

; For a set, where no elements are repeated, the subsets are equal to the subsets of
; (cdr set) plus the set of (cdr set) with (car set) prepended to each subset. This
; algorithm works recursively down to (in this case) (car set) == 3 and (cdr set) == ().
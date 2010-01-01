#lang scheme 
(require (planet schematics/schemeunit:3))

; Set representation with no duplicate entries in the list.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
        
(check-equal? (element-of-set? 1 '()) false)
(check-equal? (element-of-set? 1 '(2 3 4)) false)
(check-equal? (element-of-set? 1 '(1 2 3 4)) true)
(check-equal? (element-of-set? 1 '(1)) true)

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))
    
(check-equal? (adjoin-set 1 '()) '(1))
(check-equal? (adjoin-set 1 '(2)) '(1 2))
(check-equal? (adjoin-set 1 '(2 3 4)) '(1 2 3 4))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
          (cons (car set1)
                (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(check-equal? (intersection-set '() '()) '())
(check-equal? (intersection-set '() '(1 2 3)) '())
(check-equal? (intersection-set '(1 2 3) '()) '())
(check-equal? (intersection-set '(2) '(2 3 4 5)) '(2))
(check-equal? (intersection-set '(2 3 4 5) '(2)) '(2))
(check-equal? (intersection-set '(2 3 4 5) '(1 2 3)) '(2 3))
(check-equal? (intersection-set '(1 2 3) '(2 3 4 5)) '(2 3))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
          (union-set (cdr set1) set2))
        (else (cons (car set1)
                (union-set (cdr set1) set2)))))

(check-equal? (union-set '() '()) '())
(check-equal? (union-set '() '(1 2 3)) '(1 2 3))
(check-equal? (union-set '(1 2 3) '()) '(1 2 3))
(check-equal? (union-set '(2) '(2 3 4 5)) '(2 3 4 5))
; union-set doesn't sort.
(check-equal? (union-set '(2 3 4 5) '(2)) '(3 4 5 2))
(check-equal? (union-set '(2 3 4 5) '(1 2 3)) '(4 5 1 2 3))
(check-equal? (union-set '(1 2 3) '(2 3 4 5)) '(1 2 3 4 5))

        

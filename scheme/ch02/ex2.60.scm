#lang scheme 
(require (planet schematics/schemeunit:3))

; Set representation with duplicate entries in the list.

; Unchanged, but it will be slower due to the redundant entries.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
        
(check-equal? (element-of-set? 1 '()) false)
(check-equal? (element-of-set? 1 '(2 3 4)) false)
(check-equal? (element-of-set? 1 '(1 2 3 4)) true)
(check-equal? (element-of-set? 1 '(1)) true)

; Now just cons the element to the set, rather than checking first if x is already
; in the set. This turns the function from O(n) to O(1).
(define (adjoin-set x set) (cons x set))
    
(check-equal? (adjoin-set 1 '()) '(1))
(check-equal? (adjoin-set 1 '(1)) '(1 1))
(check-equal? (adjoin-set 1 '(2)) '(1 2))
(check-equal? (adjoin-set 1 '(1 2)) '(1 1 2))
(check-equal? (adjoin-set 1 '(2 3 4)) '(1 2 3 4))

; Must use the same implementation to find the true intersection. Will be slower
; due to the redundant entries, but the resulting set will have no duplicates.
(define (intersection-set set1 set2) (append set1 set2)
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

; Now just joins the two sets, rather than checking first for duplicates.
; This turns the function from O(n) to O(1).
(define (union-set set1 set2) (append set1 set2))

(check-equal? (union-set '() '()) '())
(check-equal? (union-set '() '(1 2 3)) '(1 2 3))
(check-equal? (union-set '(1 2 3) '()) '(1 2 3))
(check-equal? (union-set '(2) '(2 3 4 5)) '(2 2 3 4 5))
; union-set doesn't sort.
(check-equal? (union-set '(2 3 4 5) '(2)) '(2 3 4 5 2))
(check-equal? (union-set '(2 3 4 5) '(1 2 3)) '(2 3 4 5 1 2 3))
(check-equal? (union-set '(1 2 3) '(2 3 4 5)) '(1 2 3 2 3 4 5))

; Since the representations of a set are no longer unique, e.g., 
; '(1 1 1 3) represents the same set as '(1 2), we really need an
; equality method.
(define (equal-sets? set1 set2)
  (define (check-one s1 s2)
    (cond ((null? s1) true)
          ((element-of-set? (car s1) s2)
            (check-one (cdr s1) s2))
          (else false)))
  (and (check-one set1 set2) (check-one set2 set1)))
        
(check-equal? (equal-sets? '() '()) true)
(check-equal? (equal-sets? '() '(1)) false)
(check-equal? (equal-sets? '(1) '()) false)
(check-equal? (equal-sets? '(1) '(1)) true)
(check-equal? (equal-sets? '(1) '(1 1)) true)
(check-equal? (equal-sets? '(1) '(1 1 1)) true)
(check-equal? (equal-sets? '(1 1) '(1)) true)
(check-equal? (equal-sets? '(1 1 1) '(1)) true)
(check-equal? (equal-sets? '(1 2 3 4 5) '(1 2 3 4 5)) true)
(check-equal? (equal-sets? '(1 2 3 4 5) '(1 2 1 3 3 2 4 4 4 5 5 4 3 2 1)) true)
(check-equal? (equal-sets? '(1 2 1 3 3 2 4 4 4 5 5 4 3 2 1) '(1 2 3 4 5)) true)
        
; This implementation would be best when adjoin performance is more important 
; than the performance of all the other operations. That is, when you want to 
; add to the set in O(1) time, rather than O(n) time, and you're willing to accept
; performance of the other methods that are still O(n) or O(n*n), but with a higher
; constant value.

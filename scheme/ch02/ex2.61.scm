#lang scheme 
(require (planet schematics/schemeunit:3))

; Set representation with ordered entries (assume numbers) in the list.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
        
(check-equal? (element-of-set? 1 '()) false)
(check-equal? (element-of-set? 1 '(2 3 4)) false)
(check-equal? (element-of-set? 1 '(1 2 3 4)) true)
(check-equal? (element-of-set? 1 '(1)) true)

; This is also O(n), but on average takes 1/2 the time of adjoin-set in an
; unordered list implementation, just like element-of-set? above. This is true
; because the traversal of the existing set will stop when one of the two
; conditionals (= or <) is true.
(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((s (car set)))
        (cond ((= x s) set)
              ((< x s) (cons x set))
              (else (cons s (adjoin-set x (cdr set))))))))

(check-equal? (adjoin-set 1 '()) '(1))
(check-equal? (adjoin-set 1 '(1)) '(1))
(check-equal? (adjoin-set 1 '(2)) '(1 2))
(check-equal? (adjoin-set 1 '(1 2)) '(1 2))
(check-equal? (adjoin-set 1 '(1 2 3 4)) '(1 2 3 4))
(check-equal? (adjoin-set 2 '(1 2 3 4)) '(1 2 3 4))
(check-equal? (adjoin-set 3 '(1 2 3 4)) '(1 2 3 4))
(check-equal? (adjoin-set 4 '(1 2 3 4)) '(1 2 3 4))
(check-equal? (adjoin-set 1 '(2 3 4)) '(1 2 3 4))
(check-equal? (adjoin-set 2 '(1 3 4)) '(1 2 3 4))
(check-equal? (adjoin-set 3 '(1 2 4)) '(1 2 3 4))
(check-equal? (adjoin-set 4 '(1 2 3)) '(1 2 3 4))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
              (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2)
              (intersection-set (cdr set1) set2))
            ((< x2 x1)
              (intersection-set set1 (cdr set2)))))))

(check-equal? (intersection-set '() '()) '())
(check-equal? (intersection-set '() '(1 2 3)) '())
(check-equal? (intersection-set '(1 2 3) '()) '())
(check-equal? (intersection-set '(2) '(2 3 4 5)) '(2))
(check-equal? (intersection-set '(2 3 4 5) '(2)) '(2))
(check-equal? (intersection-set '(2 3 4 5) '(1 2 3)) '(2 3))
(check-equal? (intersection-set '(1 2 3) '(2 3 4 5)) '(2 3))

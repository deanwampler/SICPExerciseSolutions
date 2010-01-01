#lang scheme 
(require (planet schematics/schemeunit:3))

; union-set for set representation with ordered entries (assume numbers).

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
        
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                    (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                    (cons x1 (union-set (cdr set1) set2)))
                  (else
                    (cons x2 (union-set set1 (cdr set2)))))))))

(check-equal? (union-set '() '()) '())
(check-equal? (union-set '() '(1 2 3)) '(1 2 3))
(check-equal? (union-set '(1 2 3) '()) '(1 2 3))
(check-equal? (union-set '(2) '(2 3 4 5)) '(2 3 4 5))
(check-equal? (union-set '(3) '(2 3 4 5)) '(2 3 4 5))
(check-equal? (union-set '(4) '(2 3 4 5)) '(2 3 4 5))
(check-equal? (union-set '(5) '(2 3 4 5)) '(2 3 4 5))
(check-equal? (union-set '(2 3 4 5) '(2)) '(2 3 4 5))
(check-equal? (union-set '(2 3 4 5) '(3)) '(2 3 4 5))
(check-equal? (union-set '(2 3 4 5) '(4)) '(2 3 4 5))
(check-equal? (union-set '(2 3 4 5) '(5)) '(2 3 4 5))
(check-equal? (union-set '(2 3 4 5) '(1 2 3)) '(1 2 3 4 5))
(check-equal? (union-set '(1 2 3) '(2 3 4 5)) '(1 2 3 4 5))


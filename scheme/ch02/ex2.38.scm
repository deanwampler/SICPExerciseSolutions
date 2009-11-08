#lang scheme 
(require (planet schematics/schemeunit:3))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(check-equal? (fold-right / 1 (list 1 2 3)) (/ 3 2))
(check-equal? (fold-left / 1 (list 1 2 3)) (/ 1 6))
(check-equal? (fold-right list (list) (list 1 2 3)) (list 1 (list 2 (list 3 (list)))))
(check-equal? (fold-left list (list) (list 1 2 3)) (list (list (list (list) 1) 2) 3))

; For fold-right and fold-left to return the same results for any sequence, "op"
; must be commutative, e.g., + and *:

(check-equal? (fold-right + 0 (list 1 2 3 4)) 10)
(check-equal? (fold-left  + 0 (list 1 2 3 4)) 10)
(check-equal? (fold-right * 1 (list 1 2 3 4)) 24)
(check-equal? (fold-left  * 1 (list 1 2 3 4)) 24)


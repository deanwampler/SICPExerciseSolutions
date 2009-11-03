#lang scheme 
(require (planet schematics/schemeunit:3))

(define (square x) (* x x))

; Returns list in reverse order because iter prepends to the answer, so the list 
; is built up from right to left.
(define (square-list1 items)
  (define (iter things answer)
    (display things)(display ":")(display answer)(newline)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items (list)))
  
(check-equal? (square-list1 (list 1 2 3 4 5)) (list 25 16 9 4 1))

; Returns list in correct order, but actually returns a nested hierarchy of lists,
; not a flat list, because the "cons" operation prepends a list to a single element.
(define (square-list2 items)
  (define (iter things answer)
    (display things)(display ":")(display answer)(newline)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items (list)))
  
(check-equal? (square-list2 (list 1 2 3 4 5)) (cons (cons (cons (cons (cons (list) 1) 4) 9) 16) 25))

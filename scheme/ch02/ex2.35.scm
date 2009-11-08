#lang scheme 
(require (planet schematics/schemeunit:3))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves tree)
  (accumulate (lambda (node count)
                (if (not (pair? node)) 
                    (+ count 1)
                    (+ count (count-leaves node)))) 
                0 tree))
              
(define x  (cons (list 1 2) (list 3 4)))
(define xx (list x x))

(check-equal? (count-leaves x)  4)
(check-equal? (count-leaves xx) 8)

#lang scheme 
(require (planet schematics/schemeunit:3))

(define (square-tree1 tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree1 (car tree)) (square-tree1 (cdr tree))))))
      
(check-equal? 
  (square-tree1 
    (list 1 (list 2 (list 3 4) 5) (list 6 7))) 
    (list 1 (list 4 (list 9 16) 25) (list 36 49)))

(define (square-tree2 tree)
  (map (lambda (subtree) 
    (if (pair? subtree)
        (square-tree2 subtree)
        (* subtree subtree)))
    tree))

(check-equal? 
  (square-tree2 
    (list 1 (list 2 (list 3 4) 5) (list 6 7))) 
    (list 1 (list 4 (list 9 16) 25) (list 36 49)))



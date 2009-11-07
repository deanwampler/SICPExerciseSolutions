#lang scheme 
(require (planet schematics/schemeunit:3))

(define (tree-map f tree)
  (map (lambda (subtree) 
    (if (pair? subtree)
        (tree-map f subtree)
        (f subtree)))
    tree))

(define (square x) (* x x))

(define (square-tree tree)
  (tree-map square tree))

(check-equal? 
  (square-tree 
    (list 1 (list 2 (list 3 4) 5) (list 6 7))) 
    (list 1 (list 4 (list 9 16) 25) (list 36 49)))
(check-equal? (square-tree (list)) (list))
(check-equal? (square-tree (list 2)) (list 4))



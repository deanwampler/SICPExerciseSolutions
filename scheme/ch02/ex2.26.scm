#lang scheme 
(require (planet schematics/schemeunit:3))

(define x (list 1 2 3))
(define y (list 4 5 6))

(display (append x y))(newline)  ; (1 2 3 4 5 6)
(display (cons x y))(newline)    ; ((1 2 3) 4 5 6)
(display (list x y))(newline)    ; ((1 2 3) (4 5 6))

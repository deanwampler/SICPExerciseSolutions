#lang scheme 
(require (planet schematics/schemeunit:3))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))
  
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
          (element-of-set? x (left-branch set)))
        ((> x (entry set))
          (element-of-set? x (right-branch set)))))
        
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
          (make-tree (entry set)
                     (adjoin-set x (left-branch set))
                     (right-branch set)))
        ((> x (entry set))
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define tree1 (make-tree 7 
  (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '())) 
  (make-tree 9 '() (make-tree 11 '() '()))))
  
(define tree2 (make-tree 3 
  (make-tree 1 '() '())
  (make-tree 7 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 11 '() '())))))

(define tree3 (make-tree 5
  (make-tree 3 (make-tree 1 '() '()) '())
  (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '())))) 

(check-equal? (tree->list-1 tree1) (tree->list-2 tree1))
(check-equal? (tree->list-1 tree2) (tree->list-2 tree2))
(check-equal? (tree->list-1 tree3) (tree->list-2 tree3))

; a. Both tree->list functions traverse the structure depth-first, appending to
; the resulting list the (left entry right) at each level, where left and right
; will be built up from the lower layers first. Therefore, both functions produce
; the same sorted list output as the checks demonstrate.
;
; b. Both functions traverse all elements of the tree, an O(n) process, through
; the common structure of the form
;  (f (left-branch) (cons (entry) (f (right-branch) ...)
; where "f" is either tree->list-1 or copy-to-list (for tree->list-2). 
; Hence, these parts of the functions have roughly the same order.
; tree->list-2 traverses each node once as it cons the element to the result-list.
; However, tree->list-1 traverses the nodes roughly O(n) again when it appends to
; the end of lists. So, it grows ~ 2x as fast.
; Here is a rough test (it has overhead from the test itself).

(define (runtime) (current-inexact-milliseconds))

(define (time-tree->list n which-run tree->list-proc tree)
  (display "starting ")(display which-run)(display ": ")
  (let ((start-time (runtime)))
    (define (loop n)
      (tree->list-proc tree)
      (if (= n 0) 
          #t
          (loop (- n 1))))
    (loop n)
    (display (- (runtime) start-time))(newline)))
  
(time-tree->list 1000 "tree->list-1 tree1" tree->list-1 tree1)
(time-tree->list 1000 "tree->list-2 tree1" tree->list-2 tree1)
(time-tree->list 1000 "tree->list-1 tree2" tree->list-1 tree2)
(time-tree->list 1000 "tree->list-2 tree2" tree->list-2 tree2)
(time-tree->list 1000 "tree->list-1 tree3" tree->list-1 tree3)
(time-tree->list 1000 "tree->list-2 tree3" tree->list-2 tree3)
; output:
; starting tree->list-1 tree1: 0.385986328125
; starting tree->list-2 tree1: 0.14599609375
; starting tree->list-1 tree2: 0.34912109375
; starting tree->list-2 tree2: 0.180908203125
; starting tree->list-1 tree3: 0.3349609375
; starting tree->list-2 tree3: 0.14404296875

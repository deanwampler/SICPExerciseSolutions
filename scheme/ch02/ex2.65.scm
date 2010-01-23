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

; Use tree->list-2 in Ex. 2.63.
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                         result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
  
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; One way to implement the functions using what we already have is to convert the
; trees to lists, compute the union/intersection on the lists, and then convert
; back to trees. It's still O(n), although we'll make at least 3 passes through
; the data.

(define (union-set-lists set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                    (cons x1 (union-set-lists (cdr set1) (cdr set2))))
                  ((< x1 x2)
                    (cons x1 (union-set-lists (cdr set1) set2)))
                  (else
                    (cons x2 (union-set-lists set1 (cdr set2)))))))))

(define (intersection-set-lists set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
              (cons x1 (intersection-set-lists (cdr set1) (cdr set2))))
            ((< x1 x2)
              (intersection-set-lists (cdr set1) set2))
            ((< x2 x1)
              (intersection-set-lists set1 (cdr set2)))))))

(define (union-set set1-tree set2-tree)
  (list->tree (union-set-lists (tree->list set1-tree) (tree->list set2-tree))))
(define (intersection-set set1-tree set2-tree)
  (list->tree (intersection-set-lists (tree->list set1-tree) (tree->list set2-tree))))
  
; Here is the tree for (1 3 5 7 9 11) - from fig. 2.16:
;           5
;       +---+---+
;       3       9
;     +-+-+   +-+-+
;     1       7   11
;
; Here is the tree for (4 5 6 7 8):
;           6
;       +---+---+
;       5       8
;     +-+-+   +-+-+
;     4       7
;
; The resulting trees should have the following elements:
; union: (1 3 4 5 6 7 8 9 11)
;           6
;       +---+---+
;       3       8
;     +-+-+   +-+-+
;     1   4   7   9
;       +-+-+   +-+-+ 
;           5       11
;
; Note that this is equivalent to:
;           6
;       +---+---+
;       4       9
;     +-+-+   +-+-+
;     3   5   8   11
;   +-+-+   +-+-+ 
;   1       7
; However, the algorithm places unmatched items on the right, rather than the left.
; 
; intersection: (5 7)
;         5
;       +-+-+
;           7
; Similarly, the following tree would also be valid, but doesn't result:
;         7
;       +-+-+
;       5

(check-equal? 
  (union-set
    '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))
    '(6 (5 (4 () ()) ()) (7 () (8 () ()))))
  '(6 (3 (1 () ()) (4 () (5 () ()))) (8 (7 () ()) (9 () (11 () ())))))
(check-equal? 
  (intersection-set
    '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ())))
    '(6 (5 (4 () ()) ()) (7 () (8 () ()))))
  '(5 () (7 () ())))
  
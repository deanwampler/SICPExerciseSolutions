#lang scheme 
(require (planet schematics/schemeunit:3))

; Use a pair for the key-value.
(define (make-entry key value) (list key value))
(define (key tree-entry) (car tree-entry))
(define (value tree-entry) (cadr tree-entry))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

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

(define (lookup k set)
  (cond ((null? set) #f)
        ((equal? k (key (entry set))) (value (entry set)))
        ((< k (key (entry set))) (lookup k (left-branch set)))
        (else (lookup k (right-branch set)))))

; Here is the tree for (1 2 3 4 5 6 7 8 9 10), showing the keys only. We'll
; use the 10 * key as the value.
;           5
;       +---+---+
;       3       7
;     +-+-+   +-+-+
;     2   4   6   9
;   +-+-+       +-+-+ 
;   1           8   10

; Hard-code the tree...
(check-equal? 
  (lookup 7 
    '((5 50) ((3 30) ((2 20) ((1 10) () ()) ()) ((4 40) () ())) 
             ((7 70) ((6 60) () ()) ((9 90) ((8 80) () ()) ((10 100) () ())))))
  70)
(check-equal? 
  (lookup 11 
    '((5 50) ((3 30) ((2 20) ((1 10) () ()) ()) ((4 40) () ())) 
             ((7 70) ((6 60) () ()) ((9 90) ((8 80) () ()) ((10 100) () ())))))
  #f)

  
; ... then generate the tree from a list and try that (won't be the same tree...)
(define (10x-list->tree)
  (define (make-nth n result)
    (if (> n 10) 
      result
      (make-nth (+ n 1) (cons (make-entry n (* n 10)) result))))
  (list->tree (reverse (make-nth 1 '()))))

(check-equal? (lookup 7 (10x-list->tree)) 70)
(check-equal? (lookup 11 (10x-list->tree)) #f)

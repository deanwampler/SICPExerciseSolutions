#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? x)
  (eq? (car x) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
        
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
      
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
            (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (decode-1 (cdr bits) tree))
            (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
  
(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- choose-branch" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-sym subtree result)
    (if (leaf? subtree)
        (if (eq? symbol (symbol-leaf subtree)) result '())
        (let ((left-result  (encode-sym (left-branch subtree) (cons 0 result)))
              (right-result (encode-sym (right-branch subtree) (cons 1 result))))
          (cond ((not (empty? left-result)) left-result)
                ((not (empty? right-result)) right-result)
                (else '())))))
  (let ((answer (reverse (encode-sym tree '()))))
    (if (null? answer)
        (error "Could not encode symbol")
        answer)))
  
(define sample-tree
  (make-code-tree 
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree 
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))
        
(define expected-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(check-equal? (encode '(A) sample-tree) '(0))
(check-equal? (encode '(B) sample-tree) '(1 0))
(check-equal? (encode '(C) sample-tree) '(1 1 1))
(check-equal? (encode '(D) sample-tree) '(1 1 0))
(check-equal? (encode '(A D A B B C A) sample-tree) expected-message)
; (check-equal? (encode '(E) sample-tree) '(1 1 0))

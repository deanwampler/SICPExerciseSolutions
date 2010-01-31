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
      
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))  
                    
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))
                    
(check-equal? (make-leaf-set '((A 4) (B 2) (C 1) (D 1))) 
              '((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; Iterate through the set, using adjoin-set to insert at the proper location
; the new subtree that replaces the two lowest-ranked elements in the set (which
; could be either pairs or subtrees).
(define (successive-merge leaf-set)
  (if (= 1 (length leaf-set)) ; when 1, we have 1 tree, rather than a set of leaves
      leaf-set
      (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set)))))
    
; Starting with '((A 4) (B 2) (C 1) (D 1)), we should get the following encoding:
;  
;          + ((A B C D) 8)
;     +----+----+
;     + (A 4)   + ((B C D) 4)
;         +-----+-----+
;         + (B 2)     + ((D C) 2) 
;             +-------+-------+
;             + (D 1)         + (C 1)
;
; Resulting tree:
; (((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8))

(check-equal? (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
              '(((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)))
              
(check-equal? (generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
              '(((leaf A 8) ((((leaf H 1) (leaf G 1) (H G) 2) 
                ((leaf F 1) (leaf E 1) (F E) 2) (H G F E) 4) 
                (((leaf D 1) (leaf C 1) (D C) 2) (leaf B 3) (D C B) 5) 
                  (H G F E D C B) 9) (A H G F E D C B) 17)))

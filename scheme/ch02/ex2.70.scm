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
        (error "Could not encode symbol: " symbol)
        answer)))
  
;
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
                    
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; Iterate through the set, using adjoin-set to insert at the proper location
; the new subtree that replaces the two lowest-ranked elements in the set (which
; could be either pairs or subtrees).
(define (successive-merge leaf-set)
  (if (= 1 (length leaf-set)) ; when 1, we have 1 tree, rather than a set of leaves
      (car leaf-set)  ; We have ((set)), so return (set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set)))))
    
(define huffman-tree (generate-huffman-tree '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1))))

(check-equal? (encode '(a)    huffman-tree) '(1 1 0 0))
(check-equal? (encode '(boom) huffman-tree) '(1 1 0 1 1))
(check-equal? (encode '(get)  huffman-tree) '(1 1 1 1 1))
(check-equal? (encode '(job)  huffman-tree) '(1 1 1 1 0))
(check-equal? (encode '(na)   huffman-tree) '(0))
(check-equal? (encode '(sha)  huffman-tree) '(1 1 1 0))
(check-equal? (encode '(yip)  huffman-tree) '(1 0))
(check-equal? (encode '(wah)  huffman-tree) '(1 1 0 1 0))
(define message-encoding
  (encode 
    '(get a job 
      sha na na na na na na na na 
      get a job 
      sha na na na na na na na na 
      wah yip yip yip yip yip yip yip yip yip 
      sha boom)
    huffman-tree)) 
(check-equal? (length message-encoding) 84)

; So, 84 bits are required to encode this message. 
; With a fixed-length representation, we would need 3 bits (log_2(8)) times the 
; number of symbols, 37, so we would need 3 * 37 = 111.

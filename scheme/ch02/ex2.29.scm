#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (car (cdr m)))
  
(define (branch-length b)
  (car b))

(define (branch-structure b)
  (car (cdr b)))
  
(define (total-weight m)
  (define (eval-branch b)
    (let ((struct (branch-structure b)))
      (if (pair? struct)
        (total-weight struct)
        struct)))
  (cond ((empty? m) 0)
        (else (+ (eval-branch (left-branch m)) (eval-branch (right-branch m))))))
        
(define m1   (make-mobile (make-branch 2 5)  (make-branch 1 10)))
(define m2   (make-mobile (make-branch 3 4)  (make-branch 4 3)))
(define m12  (make-mobile (make-branch 5 m1) (make-branch 6 m2)))
(define m12b (make-mobile (make-branch 7 m1) (make-branch 15 m2)))
(check-equal? (total-weight m1)   15)
(check-equal? (total-weight m2)    7)
(check-equal? (total-weight m12)  22)
(check-equal? (total-weight m12b) 22)

(define (is-balanced? m)
  (define (branch-balance b)
    (let ((struct (branch-structure b))
          (len (branch-length b)))
      (if (pair? struct)
        (if (is-balanced? struct)
          (* len (total-weight struct))
          #f)
        (* len struct))))
  (cond ((empty? m) #t)
        (else (let ((l-balance (branch-balance (left-branch m)))
                    (r-balance (branch-balance (right-branch m))))
                (cond ((or (not l-balance) (not r-balance)) #f)
                      (else (= 0 (- l-balance r-balance))))))))

(check-equal? (is-balanced? m1)   #t)
(check-equal? (is-balanced? m2)   #t)
(check-equal? (is-balanced? m12)  #f)
(check-equal? (is-balanced? m12b) #t)
  
; For item d. I would only have to change these definitions, as follows:
; (define (make-mobile left right)
;   (cons left right))
;
; (define (make-branch length structure)
;   (cons length structure))
;
; (define (right-branch m)
;   (cdr m))
; 
; (define (branch-structure b)
;   (cdr b))

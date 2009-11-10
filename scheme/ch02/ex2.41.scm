#lang scheme 
(require (planet schematics/schemeunit:3))

; Recall that accumulate is really fold-right
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (flatmap proc seq)
  (fold-right append (list) (map proc seq)))
  
(define (enumerate-interval start end)
  (define (iter result n)
    (if (> n end)
      result
      (iter (append result (list n)) (+ n 1))))
  (iter (list) start))

(define (unique-pairs n)
  (flatmap 
    (lambda (i)
      (map (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (unique-triplets n)
  (flatmap 
    (lambda (k) (map (lambda (pair) (cons k pair)) (unique-pairs (- k 1))))
    (enumerate-interval 1 n)))

(check-equal? (unique-triplets 6) 
              (list (list 3 2 1) 
                    (list 4 2 1) (list 4 3 1) (list 4 3 2) 
                    (list 5 2 1) (list 5 3 1) (list 5 3 2) (list 5 4 1) (list 5 4 2) (list 5 4 3)
                    (list 6 2 1) (list 6 3 1) (list 6 3 2) (list 6 4 1) (list 6 4 2) (list 6 4 3)
                    (list 6 5 1) (list 6 5 2) (list 6 5 3) (list 6 5 4) ))

; Let's generalize this to arbitrary tuples:
              
(define (unique-tuple arity n)
  (if (= arity 1)
      (map (lambda (x) (list x)) (enumerate-interval 1 n))
      (flatmap 
        (lambda (k) (map (lambda (tuple-1) (cons k tuple-1)) (unique-tuple (- arity 1) (- k 1))))
        (enumerate-interval 1 n))))

(check-equal? (unique-tuple 0 6) (list))

(check-equal? (unique-tuple 1 6)
              (list (list 1) (list 2) (list 3) (list 4) (list 5) (list 6)))

(check-equal? (unique-tuple 2 6) 
              (list (list 2 1) 
                    (list 3 1) (list 3 2) 
                    (list 4 1) (list 4 2) (list 4 3)
                    (list 5 1) (list 5 2) (list 5 3) (list 5 4)
                    (list 6 1) (list 6 2) (list 6 3) (list 6 4) (list 6 5)))

(check-equal? (unique-tuple 3 6)
              (list (list 3 2 1) 
                    (list 4 2 1) (list 4 3 1) (list 4 3 2) 
                    (list 5 2 1) (list 5 3 1) (list 5 3 2) (list 5 4 1) (list 5 4 2) (list 5 4 3)
                    (list 6 2 1) (list 6 3 1) (list 6 3 2) (list 6 4 1) (list 6 4 2) (list 6 4 3)
                    (list 6 5 1) (list 6 5 2) (list 6 5 3) (list 6 5 4) ))

(check-equal? (unique-tuple 4 5)
              (list (list 4 3 2 1)
                    (list 5 3 2 1) (list 5 4 2 1) (list 5 4 3 1) (list 5 4 3 2)))

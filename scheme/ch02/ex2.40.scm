#lang scheme 
(require (planet schematics/schemeunit:3))

; From ex 1.21
(define (square n) (* n n))

(define (smallest_divisor n) (find_divisor n 2))

(define (find_divisor n test_divisor)
  (cond ((> (square test_divisor) n) n)
        ((divides? test_divisor n) test_divisor)
        (else (find_divisor n  (+ test_divisor 1)))))

(define (divides? test_divisor n) (= (remainder n test_divisor) 0))

(define (prime? n) (= (smallest_divisor n) n))

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
    
(check-equal? (enumerate-interval 0 0)  (list 0))
(check-equal? (enumerate-interval 0 5)  (list 0 1 2 3 4 5))
(check-equal? (enumerate-interval 5 10) (list 5 6 7 8 9 10))
(check-equal? (unique-pairs 6) 
              (list (list 2 1) 
                    (list 3 1) (list 3 2) 
                    (list 4 1) (list 4 2) (list 4 3)
                    (list 5 1) (list 5 2) (list 5 3) (list 5 4)
                    (list 6 1) (list 6 2) (list 6 3) (list 6 4) (list 6 5)))
                    
(define (pair-sum pair)
  (+ (car pair) (cadr pair)))

(define (prime-sum? pair)
  (prime? (pair-sum pair)))
  
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (pair-sum pair)))
  
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
       

(check-equal? (prime-sum-pairs 6) 
              (list (list 2 1 3) 
                    (list 3 2 5) 
                    (list 4 1 5) (list 4 3 7)
                    (list 5 2 7)
                    (list 6 1 7) (list 6 5 11)))
       
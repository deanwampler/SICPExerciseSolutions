#lang scheme 
(require (planet schematics/schemeunit:3))

(define (first-denomination coin-values) (car coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (no-more? coin-values) (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values)) coin-values)))))

(define (reverse l)
  (define (rev l result)
    (cond ((null? l) result)
          (else (rev (cdr l) (cons (car l) result)))))
  (rev l '()))
              

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
           
(check-equal? (cc 100 uk-coins) 104561)

; The order of the coins doesn't matter:
(check-equal? (cc 100 us-coins) 292)
(check-equal? (cc 100 (reverse us-coins)) 292)
(check-equal? (cc 100 (list 25 10 5 1 50)) 292)
(check-equal? (cc 100 (list 10 5 1 50 25)) 292)
(check-equal? (cc 100 (list 5 1 50 25 10)) 292)
(check-equal? (cc 100 (list 1 50 25 10 5)) 292)

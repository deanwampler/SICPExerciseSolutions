#lang scheme 
(require (planet schematics/schemeunit:3))

; Modified 1.22 to add the a lambda for the (next n) function and to output time
; for both primes and non-primes.

(define (square n) (* n n))

(define (smallest_divisor n next) (find_divisor n 2 next))

(define (find_divisor n test_divisor next)
  (cond ((> (square test_divisor) n) n)
        ((divides? test_divisor n) test_divisor)
        (else (find_divisor n (next test_divisor) next))))

(define (divides? test_divisor n) (= (remainder n test_divisor) 0))

(define (prime? n next) (= (smallest_divisor n next) n))

(define (runtime) (current-inexact-milliseconds))

(define (timed-prime-test n next)
  (newline)
  (display n)
  (start-prime-test n (runtime) next))

(define (start-prime-test n start-time next)
  (let ((is-prime (prime? n next)))
    (report-prime n is-prime (- (runtime) start-time))))
    
(define (report-prime n is-prime elapsed-time)
  (display n)
  (display ": prime? ")
  (display is-prime)
  (display ": ")
  (display elapsed-time)
  (newline))
  
; Original definition of next:
(define (next1 n) (+ n 1))

; New, more efficient definition of next:
(define (next2 n) 
  (if (= n 2) 3
      (+ n 2)))

; There seems to be a startup penalty (?); Run once to pay it:
(prime? 1007 (lambda (n) (next1 n)))

; Run 3 sets of non-prime and prime values, twice to get a sense of background
; time for other processes, and with the original and new definitions of next:
(start-prime-test 1007   (runtime) (lambda (n) (next1 n)))
(start-prime-test 1007   (runtime) (lambda (n) (next1 n)))
(start-prime-test 1007   (runtime) (lambda (n) (next2 n)))
(start-prime-test 1007   (runtime) (lambda (n) (next2 n)))
(start-prime-test 1009   (runtime) (lambda (n) (next1 n)))
(start-prime-test 1009   (runtime) (lambda (n) (next1 n)))
(start-prime-test 1009   (runtime) (lambda (n) (next2 n)))
(start-prime-test 1009   (runtime) (lambda (n) (next2 n)))
(start-prime-test 10005  (runtime) (lambda (n) (next1 n)))
(start-prime-test 10005  (runtime) (lambda (n) (next1 n)))
(start-prime-test 10005  (runtime) (lambda (n) (next2 n)))
(start-prime-test 10005  (runtime) (lambda (n) (next2 n)))
(start-prime-test 10007  (runtime) (lambda (n) (next1 n)))
(start-prime-test 10007  (runtime) (lambda (n) (next1 n)))
(start-prime-test 10007  (runtime) (lambda (n) (next2 n)))
(start-prime-test 10007  (runtime) (lambda (n) (next2 n)))
(start-prime-test 100001 (runtime) (lambda (n) (next1 n)))
(start-prime-test 100001 (runtime) (lambda (n) (next1 n)))
(start-prime-test 100001 (runtime) (lambda (n) (next2 n)))
(start-prime-test 100001 (runtime) (lambda (n) (next2 n)))
(start-prime-test 100003 (runtime) (lambda (n) (next1 n)))
(start-prime-test 100003 (runtime) (lambda (n) (next1 n)))
(start-prime-test 100003 (runtime) (lambda (n) (next2 n)))
(start-prime-test 100003 (runtime) (lambda (n) (next2 n)))

; Outputs show only minor time differences with large error bars (i.e., background
; processes, etc.). So, "next" probably isn't the bottleneck.
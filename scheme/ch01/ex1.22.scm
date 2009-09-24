#lang scheme 
(require (planet schematics/schemeunit:3))

; Some modifications to support testing, rather than just printing the result, 
; although the testing isn't very useful in this case. 
; MIT scheme defines runtime. PLT Scheme doesn't apparently.

(define (square n) (* n n))

(define (smallest_divisor n) (find_divisor n 2))

(define (find_divisor n test_divisor)
  (cond ((> (square test_divisor) n) n)
        ((divides? test_divisor n) test_divisor)
        (else (find_divisor n  (+ test_divisor 1)))))

(define (divides? test_divisor n) (= (remainder n test_divisor) 0))

(define (prime? n) (= (smallest_divisor n) n))

(define (runtime) (current-inexact-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))  ; will return elapsed millis
    0))  ; PLT Scheme requires an else clause (probably wise...), we just return 0
    
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (display "  ")
  elapsed-time)
  
(define (search-for-primes start end)
  (cond ((> start end))
      (else (timed-prime-test start) (search-for-primes (+ start 2) end))))

; Prints out stuff...
(search-for-primes 1001 1101)
(search-for-primes 10001 10101)
(search-for-primes 100001 100101)
(search-for-primes 1000001 1000101)

; Runs tests: should return 0 if NOT prime (odd...).
(check-equal?     (start-prime-test 1007 0) 0)
(check-not-equal? (start-prime-test 1009 0) 0)
(check-equal?     (start-prime-test 10059 0) 0)
(check-not-equal? (start-prime-test 10061 0) 0)
(check-equal?     (start-prime-test 100067 0) 0)
(check-not-equal? (start-prime-test 100069 0) 0)
(check-equal?     (start-prime-test 1000001 0) 0)
(check-not-equal? (start-prime-test 1000003 0) 0)

; Run 12 primes again, to see the times:
(start-prime-test 1009    (runtime))
(start-prime-test 1013    (runtime))
(start-prime-test 1019    (runtime))
(start-prime-test 10007   (runtime))
(start-prime-test 10009   (runtime))
(start-prime-test 10037   (runtime))
(start-prime-test 100003  (runtime))
(start-prime-test 100019  (runtime))
(start-prime-test 100043  (runtime))
(start-prime-test 1000003 (runtime))
(start-prime-test 1000033 (runtime))
(start-prime-test 1000037 (runtime))

; Eyeballing the resulting times, they scale around 3/1 with 10*N/N and hence at 
; slightly less than (sqrt 10)
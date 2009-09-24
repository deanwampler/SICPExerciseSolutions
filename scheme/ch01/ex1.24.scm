#lang scheme 
(require (planet schematics/schemeunit:3))

; Modified 1.23 to use fast-prime.

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
            m))
        (else 
          (remainder (* base (expmod base (- exp 1) m))
            m))))
          
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (runtime) (current-inexact-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  ; hardcode 10 tries
  (let ((is-prime (fast-prime? n 10)))
    (report-prime n is-prime (- (runtime) start-time))))
    
(define (report-prime n is-prime elapsed-time)
  (display n)
  (display ": prime? ")
  (display is-prime)
  (display ": ")
  (display elapsed-time)
  (newline))
  
; Run 4 prime values four times, twice to get a sense of background
; time for other processes, and once each with the original and new definitions of next:
(start-prime-test 1009   (runtime))
(start-prime-test 1009   (runtime))
(start-prime-test 1009   (runtime))
(start-prime-test 1009   (runtime))
(start-prime-test 10007  (runtime))
(start-prime-test 10007  (runtime))
(start-prime-test 10007  (runtime))
(start-prime-test 10007  (runtime))
(start-prime-test 100003 (runtime))
(start-prime-test 100003 (runtime))
(start-prime-test 100003 (runtime))
(start-prime-test 100003 (runtime))
(start-prime-test 1000003 (runtime))
(start-prime-test 1000003 (runtime))
(start-prime-test 1000003 (runtime))
(start-prime-test 1000003 (runtime))

; Scaling appears to be roughly log10(n) (linear in 10x), but there is so much
; fluctuation that it's hard to tell for certain.
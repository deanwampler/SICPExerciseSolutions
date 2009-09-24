#lang scheme 
(require (planet schematics/schemeunit:3))

; Miller-Rabin Test

(define (square n) (* n n))

(define (mr-test base exp m)
  (cond ((= base 1) false)
        ((= base (- m 1)) false)
        (else (= (square base) (remainder 1 m)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (cond ((not (mr-test base exp m)) 0)
                (else (remainder (square (expmod base (/ exp 2) m)) m))))
        (else 
          (remainder (* base (expmod base (- exp 1) m)) m))))
          
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(check-false (fast-prime? 561  1000))
(check-false (fast-prime? 1105 1000))
(check-false (fast-prime? 1729 1000))
(check-false (fast-prime? 2465 1000))
(check-false (fast-prime? 2821 1000))
(check-false (fast-prime? 6601 1000))

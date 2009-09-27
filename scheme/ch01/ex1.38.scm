#lang scheme
(require (planet schematics/schemeunit:3))

; Euler's continued-fraction expansion for e

(define (cont-frac n d k)
  (define (cf i)
    (cond ((= i k) (/ (n i) (d i)))
          (else (/ (n i) (+ (d i) (cf (+ i 1)))))))
  (cf 1))
          
(define (cont-frac-iter n d k)
  (define (cf-iter i accum)  ; start at k and work backwards
    (cond ((= i 1) (/ (n i) (+ (d i) accum)))
          (else (cf-iter (- i 1) (/ (n i) (+ (d i) accum))))))
  (cf-iter k 0.0))

(define (denom i)
  (cond ((< i 3) i)
        ((= (modulo (- i 2) 3) 0) (+ 2 (* 2 (/ (- i 2) 3))))
        (else 1)))
(define (test-denom n)
  (display (denom n))(display ", ")
  (if (< n 20) (test-denom (+ n 1)) #t))
(test-denom 1)

(define (calc-e n)
  (+ 2 (cont-frac (lambda (i) 1.0) (lambda (i) (denom i)) n)))
  
(define (calc-e-iter n)
(+ 2 (cont-frac-iter (lambda (i) 1.0) (lambda (i) (denom i)) n)))

(define (round-to-5-places x) (/ (round (* x 100000)) 100000))

(define e 2.71828)

(display "e: ")
(display e)
(newline)
(define (try-e-calc n calc)
  (cond ((= (round-to-5-places (calc n)) e)
          (display n) (newline))
        (else (try-e-calc (+ n 1) calc))))

(try-e-calc 2 calc-e) ; returns 8

(display "n=7:   ")(calc-e  7)  ; 2.7183098591549295
(display "n=8:   ")(calc-e  8)  ; 2.718279569892473  - rounds to correct 5 decimal places
(display "n=9:   ")(calc-e  9)  ; 2.718283582089552
(display "n=10:  ")(calc-e 10)  ; 2.7182817182817183
(display "n=100: ")(calc-e 100) ; 2.7182818284590455

(try-e-calc 2 calc-e-iter) ; returns 8

(display "n=7:   ")(calc-e-iter  7)  ; 2.7183098591549295
(display "n=8:   ")(calc-e-iter  8)  ; 2.718279569892473  - rounds to correct 5 decimal places
(display "n=9:   ")(calc-e-iter  9)  ; 2.718283582089552
(display "n=10:  ")(calc-e-iter 10)  ; 2.7182817182817183
(display "n=100: ")(calc-e-iter 100) ; 2.7182818284590455


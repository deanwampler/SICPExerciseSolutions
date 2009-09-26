#lang scheme
(require (planet schematics/schemeunit:3))

; k-term finite continued fraction

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

(define (calc-inverse-phi n)
  (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) n))
  
(define (calc-inverse-phi-iter n)
  (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) n))

(define inverse-phi 0.6180339882723972)
(define inverse-phi-to-4-places 0.6180)
(define (round-to-4-places x) (/ (round (* x 10000)) 10000))
(check-equal? (round-to-4-places inverse-phi) inverse-phi-to-4-places)

(display "Inverse of phi: ")
(display inverse-phi)
(newline)
(define (try-phi-calc n calc)
  (cond ((= (round-to-4-places (calc n)) inverse-phi-to-4-places)
          (display n) (newline))
        (else (try-phi-calc (+ n 1) calc))))

(try-phi-calc 5 calc-inverse-phi) ; returns 10

(display "n=9:   ")(calc-inverse-phi  9)  ; 0.6181818181818182
(display "n=10:  ")(calc-inverse-phi 10)  ; 0.6179775280898876  - rounds to correct 4 decimal places
(display "n=11:  ")(calc-inverse-phi 11)  ; 0.6180555555555556
(display "n=12:  ")(calc-inverse-phi 12)  ; 0.6180257510729613
(display "n=13:  ")(calc-inverse-phi 13)  ; 0.6180371352785146
(display "n=100: ")(calc-inverse-phi 100) ; 0.6180339887498948

(try-phi-calc 5 calc-inverse-phi-iter) ; returns 10

(display "n=9:   ")(calc-inverse-phi-iter  9)  ; 0.6181818181818182
(display "n=10:  ")(calc-inverse-phi-iter 10)  ; 0.6179775280898876  - rounds to correct 4 decimal places
(display "n=11:  ")(calc-inverse-phi-iter 11)  ; 0.6180555555555556
(display "n=12:  ")(calc-inverse-phi-iter 12)  ; 0.6180257510729613
(display "n=13:  ")(calc-inverse-phi-iter 13)  ; 0.6180371352785146
(display "n=100: ")(calc-inverse-phi-iter 100) ; 0.6180339887498948


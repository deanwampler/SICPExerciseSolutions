#lang scheme
(require (planet schematics/schemeunit:3))

; Lambert's continued-fraction expansion for tan(x)

; Pass x as an argument:
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

(define pi-over-4 (/ 3.14159265 4))


(define (nom x i) (if (= i 1) x (- (* x x))))
(define (denom x i) (+ 1 (* 2 (- i 1))))

(define (test-nom x n)
  (display (nom x n))(display ", ")
  (if (< n 4) (test-nom x (+ n 1)) #t))
(test-nom 0.0 1)
(test-nom pi-over-4 1)

(define (test-denom n)
  (display (denom 0.0 n))(display ", ")
  (if (< n 20) (test-denom (+ n 1)) #t))
(test-denom 1)

(define (calc-tan x n)
  (cont-frac (lambda (i) (nom x i)) (lambda (i) (denom x i)) n))
  
(define (calc-tan-iter x n)
  (cont-frac-iter (lambda (i) (nom x i)) (lambda (i) (denom x i)) n))

(define (round-to-5-places x) (/ (round (* x 100000)) 100000))

(define (try-tan-calc x n calc expected )
  (cond ((= (round-to-5-places (calc x n)) expected)
          (display n) (newline))
        (else (try-tan-calc x (+ n 1) calc expected))))

(display "tan(0) = 0?")
(newline)
(try-tan-calc 0.0 1 calc-tan 0.0) ; returns 1

(check-equal? (calc-tan 0.0 1) 0.0)
(check-equal? (calc-tan-iter 0.0 1) 0.0)

(display "tan(pi/2) = 1")
(newline)
(try-tan-calc pi-over-4 2 calc-tan 1.0) ; returns 4

(display "n=1:   ")(calc-tan pi-over-4  1)  ; 0.7853981625
(display "n=2:   ")(calc-tan pi-over-4  2)  ; 0.988689238219622
(display "n=3:   ")(calc-tan pi-over-4  3)  ; 0.9997876791221029
(display "n=4:   ")(calc-tan pi-over-4  4)  ; 0.9999978666208239 = 1 to 5 places
(display "n=5:   ")(calc-tan pi-over-4  5)  ; 0.9999999847314588
(display "n=10:  ")(calc-tan pi-over-4 10)  ; 0.9999999982051034
(display "n=100: ")(calc-tan pi-over-4 100) ; 0.9999999982051034

(try-tan-calc pi-over-4 2 calc-tan-iter 1.0) ; returns 4

(display "n=1:   ")(calc-tan-iter pi-over-4  1)  ; 0.7853981625
(display "n=2:   ")(calc-tan-iter pi-over-4  2)  ; 0.988689238219622
(display "n=3:   ")(calc-tan-iter pi-over-4  3)  ; 0.9997876791221029
(display "n=4:   ")(calc-tan-iter pi-over-4  4)  ; 0.9999978666208239 = 1 to 5 places
(display "n=5:   ")(calc-tan-iter pi-over-4  5)  ; 0.9999999847314588
(display "n=10:  ")(calc-tan-iter pi-over-4 10)  ; 0.9999999982051034
(display "n=100: ")(calc-tan-iter pi-over-4 100) ; 0.9999999982051034


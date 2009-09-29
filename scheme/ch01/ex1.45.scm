#lang scheme
(require (planet schematics/schemeunit:3))

(define tolerance 0.00001)

(define (close-enough? x y)
  (< (abs (- x y)) tolerance))  ; book uses 0.001 for the half-interval discussion.

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (try guess count)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next (+ count 1)))))
  (try first-guess 1))
  

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average-nth-order-damp n f)
  (define (anod i)
    (cond ((= i 0) f)
          (else (average-damp (anod (- i 1))))))
  (anod n))

(define (nth-power x n)
  (cond ((= n 0) 1)
        (else (* x (nth-power x (- n 1))))))

(check-equal? (nth-power 2 0) 1)
(check-equal? (nth-power 2 1) 2)
(check-equal? (nth-power 2 2) 4)
(check-equal? (nth-power 2 3) 8)
(check-equal? (nth-power 2 4) 16)
(check-equal? (nth-power 2 5) 32)
(check-equal? (nth-power 3 1) 3)
(check-equal? (nth-power 3 2) 9)
(check-equal? (nth-power 3 3) 27)

(define (averaged-x-over-y-n1 damp-count n)
  (lambda (x) 
    (average-nth-order-damp damp-count 
      (lambda (y) (/ x (nth-power y (- n 1)))))))

(define (nth-root x n damp-count)
  (fixed-point ((averaged-x-over-y-n1 damp-count n) x) 1.1))

(define (try-it root n damp-count)
  (display "damp count ")
  (display damp-count)
  (display ", nth root = ")
  (display root)
  (display ", root^n = ")
  (display (nth-power root n))
  (newline))

(display "square root 2.0 = 1.414213562373095")(newline)
(try-it (nth-root 2.0 2 1) 2 1)
(try-it (nth-root 2.0 2 2) 2 2)
(try-it (nth-root 2.0 2 3) 2 3)
(try-it (nth-root 2.0 2 4) 2 4)

(display "Cube root for 2.0 = 1.259921049894873")(newline)
(try-it (nth-root 2.0 3 2) 3 2)
(try-it (nth-root 2.0 3 3) 3 3)
(try-it (nth-root 2.0 3 4) 3 4)

(display "4th root for 2.0 = 1.189207115002721")(newline)
(try-it (nth-root 2.0 4 3) 4 3)
(try-it (nth-root 2.0 4 4) 4 4)
(try-it (nth-root 2.0 4 5) 4 5)

(display "5th root for 2.0 = 1.148698354997035")(newline)
(try-it (nth-root 2.0 5 4) 5 4)
(try-it (nth-root 2.0 5 5) 5 5)
(try-it (nth-root 2.0 5 6) 5 6)

(display "6th root for 2.0 = 1.122462048309373")(newline)
(try-it (nth-root 2.0 6 5) 6 5)
(try-it (nth-root 2.0 6 6) 6 6)
(try-it (nth-root 2.0 6 7) 6 7)

#lang scheme 
(require (planet schematics/schemeunit:3))

; Old sqrt calculation from ex. 1.46:

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.0001)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) tolerance))

(define (improve guess x)
  (average guess (/ x guess)))
  
(define (iterative-improve good-enough? improve)
  (lambda (guess) 
    (define (iterate guess2)
      (if (good-enough? guess2)
        guess2
        (iterate (improve guess2))))
    (iterate guess)))

(define (sqrt x)
  ((iterative-improve
    (lambda (guess) (good-enough? guess x))
    (lambda (guess) (improve guess x))) 1.0))

; Points, segments, etc. from ex. 2.2

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))
  
(define (equal-points? p1 p2)
  (and (= (x-point p1) (x-point p2)) (= (y-point p1) (y-point p2))))
  
(define (make-segment start end)
  (cons start end))
  
(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (let ((x1 (x-point (start-segment s)))
        (y1 (y-point (start-segment s)))
        (x2 (x-point (end-segment s)))
        (y2 (y-point (end-segment s))))
      (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Generic line length calculator:

(define (length-line p1 p2)
  (sqrt (+ (square (- (x-point p2) (x-point p1))) 
           (square (- (y-point p2) (y-point p1))))))
  
; Rectangles: an implementation that uses 4 points.

; counterclockwise - but doesn't assume horizontal-vertical
; p4 -- p3
; |     |
; p1 -- p2 
(define (make-rect p1 p2 p3 p4)
  (list p1 p2 p3 p4))  ; list is introduced around page 100

(define (height-rect r)
  (let ((p4 (car (cdr (cdr (cdr r)))))
        (p1 (car r)))
        (length-line p4 p1)))

(define (width-rect r)
  (let ((p2 (car (cdr r)))
        (p1 (car r)))
        (length-line p2 p1)))
  
; pass in the functions to calculate the height and width, so we can configure
; them using different rectangle implementations.

(define (area-rect height-calc width-calc r)
  (* (height-calc r) (width-calc r)))
  
(define (perimeter-rect height-calc width-calc r)
  (* 2 (+ (height-calc r) (width-calc r))))

(define zero-zero (make-point  0.0 0.0))
(define zero-two  (make-point  0.0 2.0))
(define two-zero  (make-point  2.0 0.0))
(define two-two   (make-point  2.0 2.0))
(define m-one-one (make-point -1.0 1.0))
(define one-one   (make-point  1.0 1.0))

(define rect1 (make-rect zero-zero two-zero two-two zero-two))
(check-= (width-rect     rect1) 2 tolerance)
(check-= (height-rect    rect1) 2 tolerance)
(check-= (perimeter-rect height-rect width-rect rect1) 8 tolerance)
(check-= (area-rect      height-rect width-rect rect1) 4 tolerance)

(define rect2 (make-rect zero-zero one-one zero-two m-one-one))
(define sqrt-two (sqrt 2.0))
(check-= (width-rect     rect2) sqrt-two tolerance)
(check-= (height-rect    rect2) sqrt-two tolerance)
(check-= (perimeter-rect height-rect width-rect rect2) (* 4.0 sqrt-two) tolerance)
(check-= (area-rect      height-rect width-rect rect2) 2 tolerance)

; Rectangles: 2nd implementation that uses 2 segments. 1st is assumed to be the
; more vertical; its length is taken to be the height. The 2nd is assumed to be
; the more horizontal; its length is the width.
; Area and perimeter methods unchanged.

; start point for both segments must be the same.
(define (make-rect-segments s1 s2)
  (if (equal-points? (start-segment s1) (start-segment s2))
    (cons s1 s2)
    (error "must use segments with the same starting points.")))

(define (height-rect-segment r)
  (let ((p2 (start-segment (car r)))
        (p1 (end-segment   (car r))))
        (length-line p1 p2)))

(define (width-rect-segment r)
  (let ((p2 (start-segment (cdr r)))
        (p1 (end-segment   (cdr r))))
        (length-line p1 p2)))
  
(define rect3 (make-rect-segments 
  (make-segment zero-zero zero-two) (make-segment zero-zero two-zero)))
(check-= (width-rect-segment  rect3) 2 tolerance)
(check-= (height-rect-segment rect3) 2 tolerance)
(check-= (perimeter-rect height-rect-segment width-rect-segment rect3) 8 tolerance)
(check-= (area-rect      height-rect-segment width-rect-segment rect3) 4 tolerance)

(define rect4 (make-rect-segments 
  (make-segment zero-zero m-one-one) (make-segment zero-zero one-one)))
(check-= (width-rect-segment  rect4) sqrt-two tolerance)
(check-= (height-rect-segment rect4) sqrt-two tolerance)
(check-= (perimeter-rect height-rect-segment width-rect-segment rect4) (* 4.0 sqrt-two) tolerance)
(check-= (area-rect      height-rect-segment width-rect-segment rect4) 2 tolerance)



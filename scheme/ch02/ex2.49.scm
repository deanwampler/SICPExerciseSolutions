#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-vect x y) (cons x y))
  
(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect factor v)
  (make-vect (* factor (xcor-vect v))
             (* factor (ycor-vect v))))

; If start-point and end-point are each (x,y) pairs, then they are already vectors
; as defined by make-vect above. We make a list of the points.
(define (make-segment start-point end-point)  
  (list start-point end-point))

(define (start-segment s) (car s))
  
(define (end-segment s) (cadr s))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
  
(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

; For the exercise, draw-line prints a list of the line ends.
(define (draw-line start end)
  (display (list start end)))
  

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))
                            
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))
          
(define (outline->painter frame)
  (let ((zero-zero-to-one-zero (make-segment (make-vect 0 0) (make-vect 1 0)))
        (one-zero-to-one-one   (make-segment (make-vect 1 0) (make-vect 1 1)))
        (one-one-to-zero-one   (make-segment (make-vect 1 1) (make-vect 0 1)))
        (zero-one-to-zero-zero (make-segment (make-vect 0 1) (make-vect 0 0))))
    ((segments->painter (list zero-zero-to-one-zero one-zero-to-one-one 
                            one-one-to-zero-one zero-one-to-zero-zero)) frame)))
                            
(define origin-1 (make-vect  0 0))
(define edge1-1  (make-vect  1 0)) ; edges are not relative to the origin values.
(define edge2-1  (make-vect  0 1))

(define origin-2 (make-vect -1 1))
(define edge1-2  (make-vect  2 1))
(define edge2-2  (make-vect -1 1))

(outline->painter (make-frame origin-1 edge1-1 edge2-1))
(newline)
(outline->painter (make-frame origin-2 edge1-1 edge2-1))
(newline)

(outline->painter (make-frame origin-1 edge1-2 edge2-2))
(newline)
(outline->painter (make-frame origin-2 edge1-2 edge2-2))
(newline)

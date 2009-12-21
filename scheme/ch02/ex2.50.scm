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

; For the exercise, draw-line appends the line end points to a string
; using the "open-output-string" method.
; (see http://docs.plt-scheme.org/reference/stringport.html)
(define output (open-output-string))
(define (draw-line start end)
  (display (list start end) output))

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
          
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))
                      
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2
                     
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

(define (rotate-counterclockwise-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)   ; new origin
                     (make-vect 0.0 1.0)   ; new end of edge1
                     (make-vect 1.0 0.0))) ; new end of edge2

(define (rotate-counterclockwise-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

; triangle with points at (0, 0), (1, .5) and (.5, 1).
(define (triangle1->painter frame)
  (let ((zero-zero-to-one-point-five 
          (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.5)))
        (one-point-five-to-point-five-one 
          (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0)))
        (point-five-one-to-zero-zero 
          (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.0))))
    ((segments->painter 
      (list zero-zero-to-one-point-five 
            one-point-five-to-point-five-one
            point-five-one-to-zero-zero)) frame)))
                            
; triangle with points at (0, 1), (1, .5) and (.5, 0), which is triangle1 
; flipped vertically.
(define (triangle1-vert->painter frame)
  (let ((zero-one-to-one-point-five 
          (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.5)))
        (one-point-five-to-point-five-zero
          (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0)))
        (point-five-zero-to-zero-one 
          (make-segment (make-vect 0.5 0.0) (make-vect 0.0 1.0))))
    ((segments->painter 
      (list zero-one-to-one-point-five 
            one-point-five-to-point-five-zero
            point-five-zero-to-zero-one)) frame)))
                            
; triangle with points at (1, 0), (0, .5) and (.5, 1), which is triangle1 
; flipped horizontally.
(define (triangle1-horiz->painter frame)
  (let ((one-zero-to-zero-point-five 
          (make-segment (make-vect 1.0 0.0) (make-vect 0.0 0.5)))
        (zero-point-five-to-point-five-one
          (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0)))
        (point-five-one-to-one-zero 
          (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.0))))
    ((segments->painter 
      (list one-zero-to-zero-point-five 
            zero-point-five-to-point-five-one
            point-five-one-to-one-zero)) frame)))


(define origin (make-vect  0 0))
(define edge1  (make-vect  1 0)) ; edges are not relative to the origin values.
(define edge2  (make-vect  0 1))
(define frame (make-frame origin edge1 edge2))

((flip-vert  triangle1->painter) frame)
(let ((result1 (get-output-bytes output #t)))
  (triangle1-vert->painter frame)
  (let ((result2 (get-output-bytes output #t)))
    (check-equal? result1 result2)))

((flip-horiz  triangle1->painter) frame)
(let ((result1 (get-output-bytes output #t)))
  (triangle1-horiz->painter frame)
  (let ((result2 (get-output-bytes output #t)))
    (check-equal? result1 result2)))

; triangle with points at (1, 1), (0, .5) and (.5, 0), which is triangle1 
; rotated 180 degrees.
(define (triangle1-180->painter frame)
  (let ((one-one-to-zero-point-five 
          (make-segment (make-vect 1.0 1.0) (make-vect 0.0 0.5)))
        (zero-point-five-to-point-five-zero
          (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0)))
        (point-five-zero-to-one-one 
          (make-segment (make-vect 0.5 0.0) (make-vect 1.0 1.0))))
    ((segments->painter 
      (list one-one-to-zero-point-five 
            zero-point-five-to-point-five-zero
            point-five-zero-to-one-one)) frame)))

; triangle with points at (0, 1), (.5, 0) and (1, .5), which is triangle1 
; rotated counterclockwise 270 degrees.
(define (triangle1-270->painter frame)
  (let ((zero-one-to-point-five-zero 
          (make-segment (make-vect 0.0 1.0) (make-vect 0.5 0.0)))
        (point-five-zero-to-one-point-five
          (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5)))
        (one-point-five-to-zero-one 
          (make-segment (make-vect 1.0 0.5) (make-vect 0.0 1.0))))
    ((segments->painter 
      (list zero-one-to-point-five-zero
            point-five-zero-to-one-point-five
            one-point-five-to-zero-one)) frame)))

((rotate-counterclockwise-180  triangle1->painter) frame)
(let ((result1 (get-output-bytes output #t)))
  (triangle1-180->painter frame)
  (let ((result2 (get-output-bytes output #t)))
    (check-equal? result1 result2)))

((rotate-counterclockwise-270  triangle1->painter) frame)
(let ((result1 (get-output-bytes output #t)))
  (triangle1-270->painter frame)
  (let ((result2 (get-output-bytes output #t)))
    (check-equal? result1 result2)))

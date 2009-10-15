#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (make-center-percent center percent)
  (let ((half-delta (/ (* center percent) 100.0)))
    (make-interval (- center half-delta) (+ center half-delta))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
  
(define (percent i)
  (let ((half-delta (/ (- (upper-bound i) (lower-bound i)) 2.0)))
    (* (/ half-delta (+ (lower-bound i) half-delta)) 100.0)))
  
; mul-interval definition from ex2.11, if you assume that no negative lower and
; and upper bounds are ever negative.
(define (mul-interval x y)
  (make-interval (* (lower-bound x) (lower-bound y))
                 (* (upper-bound x) (upper-bound y))))

; From make-center-percent, this can be rewritten in terms of center and percent:
(define (mul-interval x y)
  (let ((center-x (center x))
        (center-y (center y))
        (percent-x (percent x))
        (percent-y (percent y))
        (half-delta-x (/ (* center-x percent-x) 100.0))
        (half-delta-y (/ (* center-y percent-y) 100.0)))
    (make-interval (* (- center-x half-delta-x) (- center-y half-delta-y))
                   (* (+ center-x half-delta-x) (+ center-x half-delta-y)))))

; where *-x are the center and half-delta for x, same for y, and half-delta is
; as defined in make-center-percent. rearranging terms
(define (mul-interval x y)
  (let ((center-x (center x))
        (center-y (center y))
        (percent-x (percent x))
        (percent-y (percent y))
        (half-delta-x (/ (* center-x percent-x) 100.0))
        (half-delta-y (/ (* center-y percent-y) 100.0)))
    (make-interval (+ (* center-x center-y) (- (* center-x half-delta-y))
                    (- (* center-y half-delta-x)) (* half-delta-x half-delta-y))
                   (+ (* center-x center-y) (* center-x half-delta-y)
                    (* center-y half-delta-x) (* half-delta-x half-delta-y)))))

; Since half-delta is proportional to the percentage, if the percentage is very
; small for both x and y, then the term (* half-delta-x half-delta-y) is very
; small compared to the other terms. So, it can be dropped in an approximation.
(define (mul-interval x y)
  (let ((center-x (center x))
        (center-y (center y))
        (percent-x (percent x))
        (percent-y (percent y))
        (half-delta-x (/ (* center-x percent-x) 100.0))
        (half-delta-y (/ (* center-y percent-y) 100.0)))
    (make-interval (+ (* center-x center-y) (- (* center-x half-delta-y))
                      (- (* center-y half-delta-x)))
                   (+ (* center-x center-y) (* center-x half-delta-y)
                      (* center-y half-delta-x)))))

; From this, we can define mul-interval-center-percent that takes centers and 
; percents, instead of x and y:
(define (mul-interval-center-percent center-x percent-x center-y percent-y)
  (let ((half-delta-x (/ (* center-x percent-x) 100.0))
        (half-delta-y (/ (* center-y percent-y) 100.0)))
    (make-interval (+ (* center-x center-y) (- (* center-x half-delta-y))
                      (- (* center-y half-delta-x)))
                   (+ (* center-x center-y) (* center-x half-delta-y)
                      (* center-y half-delta-x)))))
(define (mul-interval-center-percent center-x percent-x center-y percent-y)
  (make-interval (+ (* center-x center-y) (- (* center-x (/ (* center-y percent-y) 100.0)))
                    (- (* center-y (/ (* center-x percent-x) 100.0))))
                 (+ (* center-x center-y) (* center-x (/ (* center-y percent-y) 100.0))
                    (* center-y (/ (* center-x percent-x) 100.0)))))
(define (mul-interval-center-percent center-x percent-x center-y percent-y)
  (let ((center-xy  (* center-x center-y))
        (half-delta-xy (+ (* center-x (/ (* center-y percent-y) 100.0)) 
                          (* center-y (/ (* center-x percent-x) 100.0)))))
    (make-interval (- center-xy half-delta-xy) (+ center-xy half-delta-xy))))
(define (mul-interval-center-percent center-x percent-x center-y percent-y)
  (let ((center-xy  (* center-x center-y))
        (percent-xy (+ percent-x percent-y))
        (half-delta-xy (/ (* center-xy percent-xy) 100.0)))
    (make-interval (- center-xy half-delta-xy) (+ center-xy half-delta-xy))))

; Recall that make-center-percenter is defined:
(define (make-center-percent center percent)
  (let ((half-delta (/ (* center percent) 100.0)))
    (make-interval (- center half-delta) (+ center half-delta))))

(define (mul-interval-center-percent center-x percent-x center-y percent-y)
  (let ((center-xy  (* center-x center-y))
        (percent-xy (+ percent-x percent-y)))
    (make-center-percent (center-xy percent-xy))))

; Therefore, percent-xy is approximately 
; (define percent-xy (+ percent-x percent-y))
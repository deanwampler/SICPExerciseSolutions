#lang scheme 
(require (planet schematics/schemeunit:3))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (div-interval x y)
  (cond ((or (= 0 (lower-bound y)) (= 0 (upper-bound y))) error "div by zero!")
        (else 
          (mul-interval x (make-interval (/ 1.0 (upper-bound y)) 
                                         (/ 1.0 (lower-bound y)))))))

(define (mul-interval x y)
  (define (bounds lx ux ly uy)
    (and (lx (lower-bound x) 0) (ux (upper-bound x) 0) 
         (ly (lower-bound y) 0) (uy (upper-bound y) 0)))
  (define (mk-interval a b c d)
    (make-interval (* a b) (* c d))) 

  (cond 
    ((bounds < < < <)
      (mk-interval (upper-bound x) (upper-bound y) (lower-bound x) (lower-bound y)))
    ((bounds < < < >=)
      (mk-interval (lower-bound x) (upper-bound y) (lower-bound x) (lower-bound y)))
    ((bounds < < >= >=)
      (mk-interval (lower-bound x) (upper-bound y) (upper-bound x) (lower-bound y)))
    ((bounds < >= < <)
      (mk-interval (upper-bound x) (lower-bound y) (lower-bound x) (lower-bound y)))
    ((bounds < >= < >=)
      (let ((p1 (* (lower-bound x) (upper-bound y)))
            (p2 (* (upper-bound x) (lower-bound y)))
            (p3 (* (lower-bound x) (lower-bound y)))
            (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p1) (max p3 p4)))) 
    ((bounds < >= >= >=)
      (mk-interval (lower-bound x) (upper-bound y) (upper-bound x) (upper-bound y)))
    ((bounds >= >= < <)
      (mk-interval (upper-bound x) (upper-bound y) (upper-bound x) (lower-bound y)))
    ((bounds >= >= < >=)
      (mk-interval (upper-bound x) (lower-bound y) (upper-bound x) (upper-bound y)))
    (else   
      (mk-interval (lower-bound x) (lower-bound y) (upper-bound x) (upper-bound y)))))


(define (make-center-percent center percent)
  (let ((half-delta (/ (* center percent) 100.0)))
    (make-interval (- center half-delta) (+ center half-delta))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
  
(define (percent i)
  (let ((half-delta (/ (- (upper-bound i) (lower-bound i)) 2.0)))
    (* (/ half-delta (+ (lower-bound i) half-delta)) 100.0)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
                  
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-center-percent 100.0 1.0))
(define r2 (make-center-percent 110.0 1.0))

(display (center (mul-interval r1 r2)))(display ", ")
(display (percent (mul-interval r1 r2)))(display ", ")
(display (center (add-interval r1 r2)))(display ", ")
(display (percent (add-interval r1 r2)))(display ", ")
(newline)(newline)
(display (center (par1  r1 r2)))(display ", ")
(display (percent (par1 r1 r2)))(newline)
(display (center (par2  r1 r2)))(display ", ")
(display (percent (par2 r1 r2)))(newline)

(define r3 (make-center-percent 100.0 0.1))
(define r4 (make-center-percent 110.0 0.1))

(display (center (par1  r3 r4)))(display ", ")
(display (percent (par1 r3 r4)))(newline)
(display (center (par2  r3 r4)))(display ", ")
(display (percent (par2 r3 r4)))(newline)

(define r5 (make-center-percent 100.0 0.0))
(define r6 (make-center-percent 110.0 0.0))

(display (center (par1  r5 r6)))(display ", ")
(display (percent (par1 r5 r6)))(newline)
(display (center (par2  r5 r6)))(display ", ")
(display (percent (par2 r5 r6)))(newline)

;(check-equal? (center  (par1 r1 r2))  (center  (par2 r1 r2)))
;(check-equal? (percent (par1 r1 r2))  (percent (par2 r1 r2)))


#lang scheme 
(require (planet schematics/schemeunit:3))
 
(define (f a b c)
  (cond ((> a b) (+ (* a a) (cond ((> b c) (* b b))
                                  (else (* c c)))))
        (else (+ (* b b)    (cond ((> a c) (* a a))
                                  (else (* c c)))))
  ))
  
(check-equal? (f 2 3 4) 25 "Should pick 3 and 4")
(check-equal? (f 2 4 3) 25)
(check-equal? (f 4 3 2) 25)
(check-equal? (f 3 4 2) 25)
(check-equal? (f 4 2 3) 25)
(check-equal? (f 3 2 4) 25)

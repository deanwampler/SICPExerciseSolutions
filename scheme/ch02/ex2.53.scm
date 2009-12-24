#lang scheme 
(require (planet schematics/schemeunit:3))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(list 'a 'b 'c)                         ; => (a b c)
(list (list 'george))                   ; => ((george))
(cdr '((x1 x2) (y1 y2)))                ; => ((y1 y2))
(cadr '((x1 x2) (y1 y2)))               ; => (y1 y2)
(pair? (car '(a short list)))           ; => #f
(memq 'red '((red shoes) (blue socks))) ; => #f
(memq 'red '(red shoes blue socks))     ; => (red shoes blue socks)

(check-equal? '(a b c) (list 'a 'b 'c))
(check-equal? '((george)) (list (list 'george)))
(check-equal? '((y1 y2)) (cdr '((x1 x2) (y1 y2))))
(check-equal? '(y1 y2) (cadr '((x1 x2) (y1 y2))))
(check-equal? #f (pair? (car '(a short list))))
(check-equal? #f (memq 'red '((red shoes) (blue socks))))
(check-equal? '(red shoes blue socks) (memq 'red '(red shoes blue socks)))

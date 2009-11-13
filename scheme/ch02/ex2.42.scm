#lang scheme 
(require (planet schematics/schemeunit:3))

; Recall that accumulate is really fold-right
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (flatmap proc seq)
  (fold-right append (list) (map proc seq)))
  
(define (enumerate-interval start end)
  (define (iter result n)
    (if (> n end)
      result
      (iter (append result (list n)) (+ n 1))))
  (iter (list) start))

(define empty-board (list (list)))

; Represent each position as a list of rows. The position in
; the list is the column, in reverse, with both rows and columns counting from 1.
(define (adjoin-position new-row col rest-of-queens)
  (map (lambda (position) 
    (cons new-row position)) rest-of-queens))

; By definition, this is the only queen in the k'th column, so we just
; check rows and diagonals for previous columns.
(define (safe? k position)
  (let ((p (car position)) (new-row (caar position)))
    (define (check-rows pos)
      (cond ((null? pos) #t)
            ((= new-row (car pos)) #f)
            (else (check-rows (cdr pos)))))
    (define (check-diags n pos)
      (cond ((null? pos) #t)
            ((or  (= (car pos) (+ new-row n)) (= (car pos) (- new-row n))) #f)
            (else (check-diags (+ n 1) (cdr pos)))))
    (and (check-rows (cdr p)) (check-diags 1 (cdr p)))))
  
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter 
          (lambda (position) (safe? k position))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
    (queen-cols board-size))

(define (test-queens n expected-n)
  (let ((queens-n (queens n))) 
    (display "board size = ")(display n)
    (display ", # solutions = ")(display (length queens-n))(display ": ")
    (display queens-n)(newline)
    (check-equal? (length queens-n) expected-n)))
    
(map (lambda (n_e) (test-queens (car n_e) (cadr n_e))) 
  (list (list 0 0) (list 1 1) (list 2 0) (list 3 0) 
  (list 4 2) (list 5 10) (list 6 4) (list 7 40) (list 8 92)))

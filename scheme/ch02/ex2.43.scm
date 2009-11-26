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
  
; Correct solution from ex. 2.42:
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

; Louis' way:
(define (queens-louis board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter 
          (lambda (position) (safe? k position))
          (flatmap
            (lambda (new-row)
              (map (lambda (rest-of-queens)
                     (adjoin-position new-row k rest-of-queens))
                    (queen-cols (- k 1))))
           (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

; From exercise 1.22
(define (runtime) (current-inexact-milliseconds))

(define (time-queens queens-func board-size title)
  (let ((start (runtime)))
    (queens-func board-size)
    (display title)(display ": size = ")(display board-size)
    (display ", time = ")(display (- (runtime) start))(newline)))

(time-queens queens       2 "queens")
(time-queens queens-louis 2 "louis ")
(time-queens queens       3 "queens")
(time-queens queens-louis 3 "louis ")
(time-queens queens       4 "queens")
(time-queens queens-louis 4 "louis ")
(time-queens queens       5 "queens")
(time-queens queens-louis 5 "louis ")
; The book says the Louis didn't wait for his 6x6 case to finish, but on my 3 GHz
; MacBook Pro (2009), the 7x7 case finishes in under 1 sec. and the 8x8 case 
; finishes in 20.5 seconds!
(time-queens queens       6 "queens")
(time-queens queens-louis 6 "louis ")
(time-queens queens       7 "queens")
(time-queens queens-louis 7 "louis ")
(time-queens queens       8 "queens")
(time-queens queens-louis 8 "louis ")

; Output. Note that for size < 4, Louis' algorithm is actually faster.
; queens: size = 2, time = 0.434814453125
; louis : size = 2, time = 0.258056640625
; queens: size = 3, time = 0.071044921875
; louis : size = 3, time = 0.031005859375
; queens: size = 4, time = 0.074951171875
; louis : size = 4, time = 0.246826171875
; queens: size = 5, time = 0.10986328125
; louis : size = 5, time = 2.881103515625
; queens: size = 6, time = 0.301025390625
; louis : size = 6, time = 58.820068359375
; queens: size = 7, time = 1.261962890625
; louis : size = 7, time = 936.68896484375
; queens: size = 8, time = 7.264892578125
; louis : size = 8, time = 20719.5869140625

; Look at queens/louis for several sizes N and compare to (N-1)!
; N  (N-1)!  q/l      Comments
; ==================================
; 8  5200    2838.3   ~ 7!/2  (ratio is 1.78)
; 7  720     720.5    almost exact!
; 6  120     196      ratio of ~1.6
; 5  24      28.8     close
; 4  6       3        ratio of 2
; 
; So, Louis' algorithm is O((N-1)! * T) for N >= 4 or so. 
; Can we explain it heuristically. Well, the correct solution calls 
; (queens-col (- k 1)) N times, once each for k = N down to 1
; (zero actually, but that's essentially a no-op). Louis' version calls
; it N times, while iterating through (enumerate-interval 1 N). So it 
; calls queens-col roughly N! times, which is N! - N times more often 
; than the correct solution, that is (N-1)! times.


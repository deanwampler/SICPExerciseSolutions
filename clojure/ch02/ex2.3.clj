(ns sicp.ch02 (:use clojure.contrib.test-is))
(use '[clojure.contrib.except :only (throw-if)])

; Old sqrt calculation from ex. 1.46:

(defn square [x] (* x x))

(defn average [x y]
  (/ (+ x y) 2))

(def tolerance 0.0001)

(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x)) tolerance))

(defn improve [guess x]
  (average guess (/ x guess)))
  
(defn iterative-improve [good-enough? improve]
  (fn [guess] 
    (loop [guess2 guess]
      (if (good-enough? guess2)
        guess2
        (recur (improve guess2))))))

(defn sqrt [x]
  ((iterative-improve
    #(good-enough? % x) #(improve % x)) 1.0))

; Points, segments, etc. from ex. 2.2

(defn make-point [x y] [x y])

(defn x-point [p] (get p 0))
(defn y-point [p] (get p 1))
  
(defn equal-points? [p1 p2]
  (and (= (x-point p1) (x-point p2)) (= (y-point p1) (y-point p2))))

(defn make-segment [start end] [start end])
  
(defn start-segment [s] (get s 0))
(defn end-segment [s]   (get s 1))

(defn midpoint-segment [s]
  (let [x1 (x-point (start-segment s))
        y1 (y-point (start-segment s))
        x2 (x-point (end-segment s))
        y2 (y-point (end-segment s))]
      (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

(defn print-point [p] (println (format "(%f,%f)" (x-point p) (y-point p))))

; Generic line length calculator:

(defn length-line [p1 p2]
  (sqrt (+ (square (- (x-point p2) (x-point p1))) 
           (square (- (y-point p2) (y-point p1))))))
  
; Rectangles: an implementation that uses 4 points.

; counterclockwise - but doesn't assume horizontal-vertical
; p4 -- p3
; |     |
; p1 -- p2 
(defn make-rect [p1 p2 p3 p4] [p1 p2 p3 p4])

(defn height-rect [r]
  (let [p4 (get r 3)
        p1 (get r 0)]
        (length-line p4 p1)))

(defn width-rect [r]
  (let [p2 (get r 1)
        p1 (get r 0)]
        (length-line p2 p1)))
  
; pass in the functions to calculate the height and width, so we can configure
; them using different rectangle implementations.

(defn area-rect [height-calc width-calc r]
  (* (height-calc r) (width-calc r)))
  
(defn perimeter-rect [height-calc width-calc r]
  (* 2 (+ (height-calc r) (width-calc r))))

(def zero-zero (make-point  0.0 0.0))
(def zero-two  (make-point  0.0 2.0))
(def two-zero  (make-point  2.0 0.0))
(def two-two   (make-point  2.0 2.0))
(def m-one-one (make-point -1.0 1.0))
(def one-one   (make-point  1.0 1.0))

(defn close-enough? [x y]
  (< (Math/abs (- x y)) tolerance))

(def sqrt-two (sqrt 2.0))

(deftest test-rect-with-4-points
  (def rect1 (make-rect zero-zero two-zero two-two zero-two))
  (def rect2 (make-rect zero-zero one-one zero-two m-one-one))
  (is (= (close-enough? (width-rect     rect1) 2) true))
  (is (= (close-enough? (height-rect    rect1) 2) true))
  (is (= (close-enough? (perimeter-rect height-rect width-rect rect1) 8) true))
  (is (= (close-enough? (area-rect      height-rect width-rect rect1) 4) true))
  (is (= (close-enough? (width-rect     rect2) sqrt-two) true))
  (is (= (close-enough? (height-rect    rect2) sqrt-two) true))
  (is (= (close-enough? (perimeter-rect height-rect width-rect rect2) (* 4 sqrt-two)) true))
  (is (= (close-enough? (area-rect      height-rect width-rect rect2) 2) true)))

; Rectangles: 2nd implementation that uses 2 segments. 1st is assumed to be the
; more vertical; its length is taken to be the height. The 2nd is assumed to be
; the more horizontal; its length is the width.
; Area and perimeter methods unchanged.

; start point for both segments must be the same.
(defn make-rect-segments [s1 s2]
  (throw-if (= false (equal-points? (start-segment s1) (start-segment s2))) 
            (str "must use segments with the same starting points." (start-segment s1) (start-segment s2)))
  [s1 s2])

(defn height-rect-segment [r]
  (let [p2 (start-segment (get r 0))
        p1 (end-segment   (get r 0))]
        (length-line p1 p2)))

(defn width-rect-segment [r]
  (let [p2 (start-segment (get r 1))
        p1 (end-segment   (get r 1))]
        (length-line p1 p2)))
  
(deftest test-rect-with-2-segments
  (def rect3 (make-rect-segments 
    (make-segment zero-zero two-zero) (make-segment zero-zero zero-two)))
  (def rect4 (make-rect-segments 
    (make-segment zero-zero one-one) (make-segment zero-zero m-one-one)))
  (is (= (close-enough? (width-rect-segment  rect3) 2) true))
  (is (= (close-enough? (height-rect-segment rect3) 2) true))
  (is (= (close-enough? (perimeter-rect height-rect-segment width-rect-segment rect3) 8) true))
  (is (= (close-enough? (area-rect      height-rect-segment width-rect-segment rect3) 4) true))

  (is (= (close-enough? (width-rect-segment  rect4) sqrt-two) true))
  (is (= (close-enough? (height-rect-segment rect4) sqrt-two) true))
  (is (= (close-enough? (perimeter-rect height-rect-segment width-rect-segment rect4) (* 4 sqrt-two)) true))
  (is (= (close-enough? (area-rect      height-rect-segment width-rect-segment rect4) 2) true)))

(run-tests)


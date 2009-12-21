(ns sicp.ch02 (:use clojure.test))

(defn make-vect [x y] [x y])
  
(defn xcor-vect [v] (first v))

(defn ycor-vect [v] (nth v 1))
  
(defn add-vect [v1 v2]
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(defn sub-vect [v1 v2]
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(defn scale-vect [factor v]
  (make-vect (* factor (xcor-vect v))
             (* factor (ycor-vect v))))

(defn make-segment [start-point end-point]  
  [start-point end-point])

(defn start-segment [s] (first s))

(defn end-segment [s] (nth s 1))

(defn make-frame [origin edge1 edge2]
  [origin edge1 edge2])

(defn origin-frame [frame]
  (nth frame 0))

(defn edge1-frame [frame]
  (nth frame 1))

(defn edge2-frame [frame]
  (nth frame 2))

; For the exercise, draw-line appends the line end points to a string
(def output (atom ""))
(defn draw-line [start end]
  (reset! output (format "%s[%s,%s]" @output start end)))
(defn reset-output [] (reset! output ""))

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))
                            
(defn segments->painter [segment-list]
  (fn [frame]
    (loop [segments segment-list]
      (if (empty? segments) 
        '()
        (let [segment (first segments)]
          (draw-line
            ((frame-coord-map frame) (start-segment segment))
            ((frame-coord-map frame) (end-segment segment)))
          (recur (rest segments)))))))
                    
;
(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter
        (make-frame new-origin
                    (sub-vect (m corner1) new-origin)
                    (sub-vect (m corner2) new-origin))))))
                      
(defn flip-vert [painter]
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2
                     
(defn flip-horiz [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

(defn rotate-counterclockwise-180 [painter]
  (transform-painter painter
                     (make-vect 1.0 1.0)   ; new origin
                     (make-vect 0.0 1.0)   ; new end of edge1
                     (make-vect 1.0 0.0))) ; new end of edge2

(defn rotate-counterclockwise-270 [painter]
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

(defn beside [painter1 painter2]
  (let [split-point (make-vect 0.5 0.0)
        paint-left (transform-painter painter1
                                      (make-vect 0.0 0.0)
                                      split-point
                                      (make-vect 0.0 1.0))
        paint-right (transform-painter painter2
                                      split-point
                                      (make-vect 1.0 0.0)
                                      (make-vect 0.5 1.0))]
    (fn [frame]
      (paint-left  frame)
      (paint-right frame))))
          
(defn below1 [painter1 painter2]
  (let [split-point (make-vect 0.0 0.5)
        paint-bottom (transform-painter painter1
                                        (make-vect 0.0 0.0)
                                        (make-vect 1.0 0.0)
                                        split-point)
        paint-top    (transform-painter painter2
                                        split-point
                                        (make-vect 1.0 0.5)
                                        (make-vect 0.0 1.0))]
        (fn [frame]
          (paint-bottom frame)
          (paint-top    frame))))
          
(defn below2 [painter1 painter2]
  (rotate-counterclockwise-270
    (rotate-counterclockwise-180
      (beside (rotate-counterclockwise-270 painter1) (rotate-counterclockwise-270 painter2)))))


; triangle with points at (0, 0), (1, .5) and (.5, 1).
(defn triangle1->painter [frame]
  (let [zero-zero-to-one-point-five 
          (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.5))
        one-point-five-to-point-five-one 
          (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
        point-five-one-to-zero-zero 
          (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.0))]
    ((segments->painter 
      [zero-zero-to-one-point-five 
       one-point-five-to-point-five-one
       point-five-one-to-zero-zero]) frame)))
                            
; triangle with points at (0, 1), (1, .5) and (.5, 0), which is triangle1 
; flipped vertically.
(defn triangle1-vert->painter [frame]
  (let [zero-one-to-one-point-five 
          (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.5))
        one-point-five-to-point-five-zero
          (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))
        point-five-zero-to-zero-one 
          (make-segment (make-vect 0.5 0.0) (make-vect 0.0 1.0))]
    ((segments->painter 
      [zero-one-to-one-point-five 
       one-point-five-to-point-five-zero
       point-five-zero-to-zero-one]) frame)))
                            
; triangle with points at (1, 0), (0, .5) and (.5, 1), which is triangle1 
; flipped horizontally.
(defn triangle1-horiz->painter [frame]
  (let [one-zero-to-zero-point-five 
          (make-segment (make-vect 1.0 0.0) (make-vect 0.0 0.5))
        zero-point-five-to-point-five-one
          (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
        point-five-one-to-one-zero 
          (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.0))]
    ((segments->painter 
      [one-zero-to-zero-point-five 
       zero-point-five-to-point-five-one
       point-five-one-to-one-zero]) frame)))

(def origin (make-vect  0 0))
(def edge1  (make-vect  1 0)) ; edges are not relative to the origin values.
(def edge2  (make-vect  0 1))
(def frame  (make-frame origin edge1 edge2))

(deftest test-below
  ((below1 triangle1->painter triangle1-vert->painter) frame)
  (let [result1 @output]
    (reset-output)
    ((below2 triangle1->painter triangle1-vert->painter) frame)
    (let [result2 @output]
      (reset-output)
      (is (= result1 result2))))
  ((below1 triangle1->painter triangle1-horiz->painter) frame)
  (let [result1 @output]
    (reset-output)
    ((below2 triangle1->painter triangle1-horiz->painter) frame)
    (let [result2 @output]
      (reset-output)
      (is (= result1 result2)))))

(run-tests)

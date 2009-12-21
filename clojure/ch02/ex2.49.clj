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
                    
(def origin-1 (make-vect  0 0))
(def edge1-1  (make-vect  1 0)) ; edges are not relative to the origin values.
(def edge2-1  (make-vect  0 1))

(def origin-2 (make-vect -1 1))
(def edge1-2  (make-vect  2 1))
(def edge2-2  (make-vect -1 1))

(defn outline->painter [frame]
  (let [zero-zero-to-one-zero (make-segment (make-vect 0 0) (make-vect 1 0))
        one-zero-to-one-one   (make-segment (make-vect 1 0) (make-vect 1 1))
        one-one-to-zero-one   (make-segment (make-vect 1 1) (make-vect 0 1))
        zero-one-to-zero-zero (make-segment (make-vect 0 1) (make-vect 0 0))]
    ((segments->painter [zero-zero-to-one-zero one-zero-to-one-one 
                         one-one-to-zero-one zero-one-to-zero-zero]) frame)))
                            
(deftest test-outline->painter
  (outline->painter (make-frame origin-1 edge1-1 edge2-1))
  (is (= @output "[[0 0],[1 0]][[1 0],[1 1]][[1 1],[0 1]][[0 1],[0 0]]"))
  (reset-output)
  (outline->painter (make-frame origin-2 edge1-1 edge2-1))
  (is (= @output "[[-1 1],[0 1]][[0 1],[0 2]][[0 2],[-1 2]][[-1 2],[-1 1]]"))
  (reset-output)
  (outline->painter (make-frame origin-1 edge1-2 edge2-2))
  (is (= @output "[[0 0],[2 1]][[2 1],[1 2]][[1 2],[-1 1]][[-1 1],[0 0]]"))
  (reset-output)
  (outline->painter (make-frame origin-2 edge1-2 edge2-2))
  (is (= @output "[[-1 1],[1 2]][[1 2],[0 3]][[0 3],[-2 2]][[-2 2],[-1 1]]"))
  (reset-output))

(defn x->painter [frame]
  (let [zero-zero-to-one-one (make-segment (make-vect 0 0) (make-vect 1 1))
        one-zero-to-zero-one   (make-segment (make-vect 1 0) (make-vect 0 1))]
    ((segments->painter [zero-zero-to-one-one one-zero-to-zero-one]) frame)))

(deftest test-x->painter
  (x->painter (make-frame origin-1 edge1-1 edge2-1))
  (is (= @output "[[0 0],[1 1]][[1 0],[0 1]]"))
  (reset-output)
  (x->painter (make-frame origin-2 edge1-1 edge2-1))
  (is (= @output "[[-1 1],[0 2]][[0 1],[-1 2]]"))
  (reset-output)
  (x->painter (make-frame origin-1 edge1-2 edge2-2))
  (is (= @output "[[0 0],[1 2]][[2 1],[-1 1]]"))
  (reset-output)
  (x->painter (make-frame origin-2 edge1-2 edge2-2))
  (is (= @output "[[-1 1],[0 3]][[1 2],[-2 2]]"))
  (reset-output))

(defn diamond->painter [frame]
  (let [one   (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
        two   (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
        three (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
        four  (make-segment (make-vect 0 0.5) (make-vect 0.5 0))]
    ((segments->painter [one two three four]) frame)))

(deftest test-diamond->painter
  (diamond->painter (make-frame origin-1 edge1-1 edge2-1))
  (is (= @output "[[0.5 0.0],[1.0 0.5]][[1.0 0.5],[0.5 1.0]][[0.5 1.0],[0.0 0.5]][[0.0 0.5],[0.5 0.0]]"))
  (reset-output)
  (diamond->painter (make-frame origin-2 edge1-1 edge2-1))
  (is (= @output "[[-0.5 1.0],[0.0 1.5]][[0.0 1.5],[-0.5 2.0]][[-0.5 2.0],[-1.0 1.5]][[-1.0 1.5],[-0.5 1.0]]"))
  (reset-output)
  (diamond->painter (make-frame origin-1 edge1-2 edge2-2))
  (is (= @output "[[1.0 0.5],[1.5 1.5]][[1.5 1.5],[0.0 1.5]][[0.0 1.5],[-0.5 0.5]][[-0.5 0.5],[1.0 0.5]]"))
  (reset-output)
  (diamond->painter (make-frame origin-2 edge1-2 edge2-2))
  (is (= @output "[[0.0 1.5],[0.5 2.5]][[0.5 2.5],[-1.0 2.5]][[-1.0 2.5],[-1.5 1.5]][[-1.5 1.5],[0.0 1.5]]"))
  (reset-output))

(run-tests)

; skipped the wave painter
(ns sicp.ch01 (:use clojure.contrib.test-is))

; solving cubic equations

(def dx 0.00001)

(defn abs [x] (if (> x 0) x (- x)))

(defn close-enough? [x y]
  (< (abs (- x y)) dx))
  
(defn fixed-point [f first-guess]
  (loop [guess first-guess count 1]
    (let [next-guess (f guess)]
      (cond (close-enough? guess next-guess) 
              next-guess
            :else (recur next-guess (+ count 1))))))

(defn deriv [g]
  (fn [x] (/ (- (g (+ x dx)) (g x))
                dx)))
                
(defn newton-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))
  
(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))
  
(defn cubic [a b c]
  (fn [x] (+ (* x x x) (* a (* x x)) (* b x) c)))

(println (newtons-method (cubic 0 0 1) 1.0))    ; -0.9999999999999863
(println (newtons-method (cubic 0 1 0) 1.0))    ; 3.668097353908429e-17
(println (newtons-method (cubic 1 0 0) 1.0))    ; 1.1227429100448376e-05
(println (newtons-method (cubic 1 1 -14) 1.0))  ; 2.0000000000000133
(println (newtons-method (cubic 2 1 1) 1.0))    ; -1.7548776662280976
(println (newtons-method (cubic 1 2 1) 1.0))    ; -0.5698402909980529
(println (newtons-method (cubic 1 1 2) 1.0))    ; -1.3532099641952162

; Is there a method like this in test-is?? Couldn't find something like it documented.
(defn is-within [expected actual delta]
  (< (abs (- expected actual)) delta))

(deftest test-newtons-method
  (is (= true (is-within -1.0 (newtons-method (cubic 0 0 1) 1.0) dx)))
  (is (= true (is-within  0.0 (newtons-method (cubic 0 1 0) 1.0) dx)))
  ; This one probably has significant round-off errors:
  (is (= true (is-within  0.0 (newtons-method (cubic 1 0 0) 1.0) (* 10 dx))))
  (is (= true (is-within  2.0 (newtons-method (cubic 1 1 -14) 1.0) dx))))

(run-tests)

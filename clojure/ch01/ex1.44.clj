(ns sicp.ch01 (:use clojure.test))

(defn smooth [f dx]
  (fn [x] (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(deftest test-smooth
  (is (= ((smooth (fn [x] (* 3 x)) 0.5) 2.0) 6.0)))

(run-tests)

(defn compose [f g]
  (fn [x] (f (g x))))

(defn repeated [f n]
  (fn [x] (loop [g f i n]
    (if (= i 1) (g x)
        (recur (compose f g) (- i 1))))))

(def pi-over-4 (/ 3.14159265 4))
(def dx 0.005)
(defn sin [x] (Math/sin x))

(println (sin pi-over-4))
(println ((repeated (smooth sin dx) 1) pi-over-4))
(println ((repeated (smooth sin dx) 2) pi-over-4))
(println ((repeated (smooth sin dx) 4) pi-over-4))


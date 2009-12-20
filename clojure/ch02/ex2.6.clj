(ns sicp.ch02 (:use clojure.test))
(use '[clojure.contrib.except :only (throw-if)])

; Church Numerals

(def zero (fn [f] (fn [x] x)))
(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))
  

(def one (add-1 zero))
(def one (fn [f] (fn [x] (f ((zero f) x)))))
(def one (fn [f] (fn [x] (f (((fn [g] (fn [y] y)) f) x)))))
(def one (fn [f] (fn [x] (f ((fn [y] y) x)))))
(def one (fn [f] (fn [x] (f x))))


(def two (add-1 one))
(def two (fn [f] (fn [x] (f ((one f) x)))))
(def two (fn [f] (fn [x] (f (((fn [g] (fn [y] (g y))) f) x)))))
(def two (fn [f] (fn [x] (f (((fn [y] (f y))) x)))))
(def two (fn [f] (fn [x] (f (f x)))))

; general case (n>=2)
; (def n (fn [f] (fn [x] (f ... (f x) ... ))))

(defn church-numeral [n]
  (fn [f] (fn [x] 
    (defn g [m n]
      (cond (= m 0) x
            :else (f (g (- m 1))))))
    (g n)))

(defn plus [m n] (church-numeral (+ m n)))

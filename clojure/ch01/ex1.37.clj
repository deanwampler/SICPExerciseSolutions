(ns sicp.ch01 (:use clojure.contrib.test-is))

; k-term finite continued fraction

(defn cont-frac [n d k]
  (defn cf [i]
    (cond (= i k) (/ (n i) (d i))
          :else (/ (n i) (+ (d i) (cf (+ i 1))))))
  (cf 1))
          
(defn cont-frac-iter [n d k]
  (defn cf-iter [i accum]  ; start at k and work backwards
    (cond (= i 1) (/ (n i) (+ (d i) accum))
          :else (cf-iter (- i 1) (/ (n i) (+ (d i) accum)))))
  (cf-iter k 0.0))

(defn calc-inverse-phi [n]
  (cont-frac (fn [n] 1.0) (fn [n] 1.0) n))
  
(defn calc-inverse-phi-iter [n]
  (cont-frac-iter (fn [n] 1.0) (fn [n] 1.0) n))

(def inverse-phi 0.6180339882723972)
(def inverse-phi-to-4-places 0.6180)
(defn round-to-4-places [x] (/ (Math/round (* x 10000)) 10000))
(deftest test-round-to-4-places
  (is (= (round-to-4-places inverse-phi) inverse-phi-to-4-places)))

(println "Inverse of phi:" inverse-phi)
(defn try-phi-calc [n calc]
  (cond (= (round-to-4-places (calc n)) inverse-phi-to-4-places)
          (println n)
        :else (try-phi-calc (+ n 1) calc)))

(try-phi-calc 5 #(calc-inverse-phi %)) ; returns 10

(println "n=9:  " (calc-inverse-phi  9))  ; 0.6181818181818182
(println "n=10: " (calc-inverse-phi 10))  ; 0.6179775280898876  - rounds to correct 4 decimal places
(println "n=11: " (calc-inverse-phi 11))  ; 0.6180555555555556
(println "n=12: " (calc-inverse-phi 12))  ; 0.6180257510729613
(println "n=13: " (calc-inverse-phi 13))  ; 0.6180371352785146
(println "n=100:" (calc-inverse-phi 100)) ; 0.6180339887498948

(try-phi-calc 5 #(calc-inverse-phi-iter %)) ; returns 10

(println "n=9:  " (calc-inverse-phi-iter  9))  ; 0.6181818181818182
(println "n=10: " (calc-inverse-phi-iter 10))  ; 0.6179775280898876  - rounds to correct 4 decimal places
(println "n=11: " (calc-inverse-phi-iter 11))  ; 0.6180555555555556
(println "n=12: " (calc-inverse-phi-iter 12))  ; 0.6180257510729613
(println "n=13: " (calc-inverse-phi-iter 13))  ; 0.6180371352785146
(println "n=100:" (calc-inverse-phi-iter 100)) ; 0.6180339887498948


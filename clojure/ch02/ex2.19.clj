(ns sicp.ch02 (:use clojure.test))

; reverse is already defined:
(defn reverse2 [l]
  (loop [l2 l result '()]
    (cond (empty? l2) result
          :else (recur (next l2) (cons (first l2) result)))))

(defn first-denomination [coin-values] (first coin-values))
(defn except-first-denomination [coin-values] (next coin-values))
(defn no-more? [coin-values] (empty? coin-values))

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else
          (+ (cc amount (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values)) coin-values))))


(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))
           
; The order of the coins doesn't matter:
(deftest test-count-coins
  (is (= (cc 100 uk-coins) 104561))
  (is (= (cc 100 us-coins) 292))
  (is (= (cc 100 (reverse us-coins)) 292))
  (is (= (cc 100 (list 25 10 5 1 50)) 292))
  (is (= (cc 100 (list 10 5 1 50 25)) 292))
  (is (= (cc 100 (list 5 1 50 25 10)) 292))
  (is (= (cc 100 (list 1 50 25 10 5)) 292)))

(run-tests)

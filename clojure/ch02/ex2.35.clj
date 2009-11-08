(ns sicp.ch02 (:use clojure.contrib.test-is))

(defn accumulate [op initial sequence]
  (if (empty? sequence)
      initial
      (op (first sequence)
          (accumulate op initial (rest sequence)))))

(defn count-leaves [tree]
  (accumulate (fn [node count]
                (if (not (list? node)) 
                    (+ count 1)
                    (+ count (count-leaves node)))) 
                0 tree))

(def x  (list (list 1 2) (list 3 4)))
(def xx (list x x))

(deftest test-length
  (is (= (count-leaves x)  4))
  (is (= (count-leaves xx) 8)))

(run-tests)

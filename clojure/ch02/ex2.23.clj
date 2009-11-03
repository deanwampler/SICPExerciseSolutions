(ns sicp.ch02 (:use clojure.contrib.test-is))

; Version suggested on the clojure mailing list by Brian Hurt.
(defn for-each [f items]
  (loop [curr items]
    (if (empty? curr)
      nil
      (do (f (first curr)) (recur (rest curr))))))

(for-each (fn [x] (println (* x x))) (list 1 2 3 4 5))

(ns sicp.ch02 (:use clojure.contrib.test-is))

(defn for-each [f items]
  (if (not (empty? items))
      (let [] (f (first items)) (for-each f (rest items)))))

(for-each (fn [x] (println (* x x))) (list 1 2 3 4 5))

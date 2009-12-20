(ns sicp.ch02 (:use clojure.test))

; Use vectors to avoid awkward behavior of "into"
(defn subsets [s]
  (if (empty? s) 
      [[]]
      (let [rest-of (subsets (rest s))]
        (into rest-of (map (fn [x] (cons (first s) x)) rest-of)))))

(deftest test-subsets
  (is (= (subsets [1 2 3]) 
              [[] [3] [2] [2 3] [1] [1 3] [1 2] [1 2 3]])))

(run-tests)
(ns sicp.ch02 (:use clojure.test))

; Using vectors because they work better.

; Recall that accumulate is really fold-right
(defn fold-right [op initial sequence]
  (if (empty? sequence)
      initial
      (op (first sequence)
          (fold-right op initial (vec (rest sequence))))))

(defn flatmap [proc seq]
  (vec (fold-right into [] (vec (map proc seq)))))
  
(defn enumerate-interval [start end]
  (seq (loop [result [] n start]
        (if (> n end)
        result
        (recur (conj result n) (+ n 1))))))

(def empty-board [[]])

; Represent each position as a vector of rows. The position in
; the vector is the column, with both rows and columns counting from 1.
(defn adjoin-position [new-row col rest-of-queens]
  (map (fn [position] (conj position new-row)) rest-of-queens))

; By definition, this is the only queen in the k'th column, so we just
; check rows and diagonals for previous columns.
(defn safe? [k position]
  (let [p (first position) new-row (last p)]
    (defn check-row [i]
      (cond (= i k) true
            (let [i-row (nth p (- i 1)) delta (- k i)]
            ;(println "k, i, delta, i-row, new-row = " k, i delta i-row new-row)
              (or (= i-row new-row) (= i-row (+ new-row delta)) (= i-row (- new-row delta)))) false
            :else (check-row (+ i 1)))))
    (check-row 1))

(defn queens [board-size]
  (defn queen-cols [k]
    (if (= k 0)
        [empty-board]
        (vec (filter 
          (fn [position] (safe? k position))
          (flatmap
            (fn [rest-of-queens]
              (map (fn [new-row]
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1)))))))
    (queen-cols board-size))

(defn length [coll]
  (loop [result 0 c coll]
    (if (empty? c) result
        (recur (+ result 1) (rest c)))))

(loop [n 0]
  (let [queens-n (queens n) len-queens-n (length queens-n)]
    (println "board size = " n " # solutions = " len-queens-n ": " queens-n))
  (if (< n 8)
      (recur (+ n 1))))

(deftest test-queens
  (is (= 1  (length (queens 0)))) ; actually, it's zero, but we have an empty [] placeholder.
  (is (= 1  (length (queens 1))))
  (is (= 0  (length (queens 2))))
  (is (= 0  (length (queens 3))))
  (is (= 2  (length (queens 4))))
  (is (= 10 (length (queens 5))))
  (is (= 4  (length (queens 6))))
  (is (= 40 (length (queens 7))))
  (is (= 92 (length (queens 8)))))
  
(run-tests)

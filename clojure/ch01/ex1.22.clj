; skipped testing for this one; not terribly useful...
(ns sicp.ch01)

(defn square [n] (* n n))

(defn divides? [test-divisor n] (= (rem n test-divisor) 0))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n  (+ test-divisor 1))))

(defn smallest-divisor [n] (find-divisor n 2))

(defn prime? [n] (= (smallest-divisor n) n))

(defn timed-prime-test [n]
  (print (str n ": "))
  (time (if (prime? n) (println "yes") (println ""))))
    
(defn search-for-primes [start end]
  (loop [s start e end]
    (if (< s e) 
      (do (timed-prime-test start) (recur (+ s 2) e)))))
                
; prints out results:
(search-for-primes 1001 1101)
(search-for-primes 10001 10101)
(search-for-primes 100001 100101)
(search-for-primes 1000001 1000101)

; Run 12 primes:
(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

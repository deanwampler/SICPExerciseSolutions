(ns sicp.ch02 (:use clojure.test))

; Start with the ex2.57 version.

(defn =number? [exp num]
  (and (number? exp) (= exp num)))
  
(defn variable? [e] (symbol? e))
  
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))
  
(defn sum? [e]
  (and (list? e) (= (nth e 1) '+)))
  
(defn addend [e] (nth e 0))
  
(defn augend [e] (nth e 2))
  
(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list a1 '+ a2)))
  
(defn difference? [e]
  (and (list? e) (= (nth e 1) '-)))

(defn minuend [e] (nth e 0))
  
(defn subtrahend [e] (nth e 2))
  
(defn make-difference [a1 a2] 
  (cond (and (number? a1) (number? a2)) (- a1 a2)
        (=number? a1 0) (list 0 '- a2)
        (=number? a2 0) a1
        :else (list a1 '- a2)))
  
(defn product? [e]
  (and (list? e) (= (nth e 1) '*)))
  
(defn multiplier [e] (nth e 0))
  
(defn multiplicand [e] (nth e 2))
  
(defn make-product [a1 a2] 
  (cond (or (=number? a1 0) (=number? a2 0)) 0
        (=number? a1 1) a2
        (=number? a2 1) a1
        (and (number? a1) (number? a2)) (* a1 a2)
        :else (list a1 '* a2)))

(defn exponentiation? [e]
  (and (list? e) (= (nth e 1) '**)))
  
(defn base [e] (nth e 0))
  
(defn exponent [e] (nth e 2))
  
(defn make-exponentiation [a1 a2] 
  (cond (=number? a1 0) 0
        (=number? a2 0) 1
        (=number? a2 1) a1
        (and (number? a1) (number? a2)) (* a1 (make-exponentiation a1 (- a2 1)))
        :else (list a1 '** a2)))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp)
          (if (same-variable? exp var) 1 0)
        (sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var))
        (difference? exp)
          (make-difference (deriv (minuend exp) var)
                           (deriv (subtrahend exp) var))
        (product? exp)
          (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp)))
        (exponentiation? exp)
          (make-product
            (make-product (exponent exp)
                          (make-exponentiation (base exp) (make-difference (exponent exp) 1)))
            (make-product (deriv (base exp) var)
                          1))
        :else 
          (throw (RuntimeException. (format "unknown expression type -- DERIV: %s" exp)))))

(deftest test-deriv          
  (is (=  1 (deriv '(x + 3) 'x)))
  (is (=  1 (deriv '(x - 3) 'x)))
  (is (= 'y (deriv '(x * y) 'x)))
  (is (= '((x * y) + (y * (x + 3))) (deriv '((x * y) * (x + 3)) 'x)))
  (is (= '((x * y) + (y * (x - 3))) (deriv '((x * y) * (x - 3)) 'x)))
  (is (= 1 (deriv '(x ** 1) 'x)))
  (is (= '(2 * x) (deriv '(x ** 2) 'x)))
  (is (= '(3 * (x ** 2)) (deriv '(x ** 3) 'x)))
  (is (= '(4 * (x ** 3)) (deriv '(x ** 4) 'x)))
  (is (= '(n * (x ** (n - 1))) (deriv '(x ** n) 'x))))

(run-tests)
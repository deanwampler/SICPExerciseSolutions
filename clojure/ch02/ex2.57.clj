(ns sicp.ch02 (:use clojure.test))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))
  
(defn variable? [e] (symbol? e))
  
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))
  
(defn sum? [e]
  (and (list? e) (= (first e) '+)))
  
(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))
  
(defn rest-of-operands [e op]
  (loop [exp (rest (rest (rest e))) total (nth e 2)]
    (cond (empty? exp) total
          :else (recur (rest exp) (op total (first exp))))))
  
(defn addend [e] (nth e 1))

(defn augend [e] (rest-of-operands e make-sum))
  
(defn difference? [e]
  (and (list? e) (= (first e) '-)))

(defn make-difference [a1 a2] 
  (cond (and (number? a1) (number? a2)) (- a1 a2)
        (=number? a1 0) (list '- 0 a2)
        (=number? a2 0) a1
        :else (list '- a1 a2)))
  
(defn minuend [e] (nth e 1))
  
; Since all the subtrahend expressions are subtraced from the minuend, we use
; make-sum to compute them. i.e., (- x1 x2 x3 ...) => (- x1 (+ x2 x3 ...))
(defn subtrahend [e] (rest-of-operands e make-sum))
  
(defn product? [e]
  (and (list? e) (= (first e) '*)))
  
(defn make-product [a1 a2] 
  (cond (or (=number? a1 0) (=number? a2 0)) 0
        (=number? a1 1) a2
        (=number? a2 1) a1
        (and (number? a1) (number? a2)) (* a1 a2)
        :else (list '* a1 a2)))

(defn multiplier [e] (nth e 1))
  
(defn multiplicand [e] (rest-of-operands e make-product))
  
(defn exponentiation? [e]
  (and (list? e) (= (first e) '**)))
  
(defn make-exponentiation [a1 a2] 
  (cond (=number? a1 0) 0
        (=number? a2 0) 1
        (=number? a2 1) a1
        (and (number? a1) (number? a2)) (* a1 (make-exponentiation a1 (- a2 1)))
        :else (list '** a1 a2)))

(defn base [e] (nth e 1))
  
; We multiple the exponents
(defn exponent [e] (rest-of-operands e make-product))
  
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
  (is (=  1 (deriv '(+ x 3) 'x)))
  (is (=  1 (deriv '(+ x 3 4) 'x)))
  (is (=  2 (deriv '(+ x x 3) 'x)))
  (is (=  1 (deriv '(- x 3) 'x)))
  (is (=  0 (deriv '(- x x 3) 'x)))
  (is (= -2 (deriv '(- x x x x 3) 'x)))
  (is (= 'y (deriv '(* x y) 'x)))
  (is (= '(+ (* x y) (* x y)) (deriv '(* x x y) 'x))) ; = '(* 2 x y)
  (is (= '(+ (* x y) (* y (+ x 3))) (deriv '(* (* x y) (+ x 3)) 'x)))
  (is (= '(+ (* x y) (* y (+ x 3))) (deriv '(* x y (+ x 3)) 'x)))
  (is (= '(+ (* x y) (* y (- x 3))) (deriv '(* (* x y) (- x 3)) 'x)))
  (is (= '(+ (* x y) (* y (- x 3))) (deriv '(* x y (- x 3)) 'x)))
  (is (= 1 (deriv '(** x 1) 'x)))
  (is (= '(* 2 x) (deriv '(** x 2) 'x)))
  (is (= '(* 6 (** x 5)) (deriv '(** x 2 3) 'x)))
  (is (= '(* 3 (** x 2)) (deriv '(** x 3) 'x)))
  (is (= '(* 4 (** x 3)) (deriv '(** x 4) 'x)))
  (is (= '(* n (** x (- n 1))) (deriv '(** x n) 'x))))

(run-tests)
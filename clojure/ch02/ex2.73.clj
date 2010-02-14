(ns sicp.ch02 (:use clojure.test))

; Adapted from Ex. 2.57. One key change. Before, the first operand was "cadr" of
; the expression and the operator was the "car". Now the operator is removed 
; before calling "addend", etc., so the first operand is "car" and the rest of
; them are "cdr".

(defn =number? [exp num]
  (and (number? exp) (= exp num)))
  
(defn variable? [e] (symbol? e))
  
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))
  
(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))
  
(defn rest-of-operands [e op]
  (loop [exp (rest (rest e)) total (nth e 1)]
    (cond (empty? exp) total
          :else (recur (rest exp) (op total (first exp))))))
  
(defn addend [e] (nth e 0))

(defn augend [e] (rest-of-operands e make-sum))
  
(defn make-difference [a1 a2] 
  (cond (and (number? a1) (number? a2)) (- a1 a2)
        (=number? a1 0) (list '- 0 a2)
        (=number? a2 0) a1
        :else (list '- a1 a2)))
  
(defn minuend [e] (nth e 0))
  
; Since all the subtrahend expressions are subtraced from the minuend, we use
; make-sum to compute them. i.e., (- x1 x2 x3 ...) => (- x1 (+ x2 x3 ...))
(defn subtrahend [e] (rest-of-operands e make-sum))
  
(defn make-product [a1 a2] 
  (cond (or (=number? a1 0) (=number? a2 0)) 0
        (=number? a1 1) a2
        (=number? a2 1) a1
        (and (number? a1) (number? a2)) (* a1 a2)
        :else (list '* a1 a2)))

(defn multiplier [e] (nth e 0))
  
(defn multiplicand [e] (rest-of-operands e make-product))
  
(defn make-exponentiation [a1 a2] 
  (cond (=number? a1 0) 0
        (=number? a2 0) 1
        (=number? a2 1) a1
        (and (number? a1) (number? a2)) (* a1 (make-exponentiation a1 (- a2 1)))
        :else (list '** a1 a2)))

(defn base [e] (nth e 0))
  
; We multiple the exponents
(defn exponent [e] (rest-of-operands e make-product))
  
; a. All the operators have uniform behavior; they are applied to a list of
; operands, which are numbers, variables, or arithmetic combinations of them
; with operators. Numbers and variables are "leaf nodes" in expression trees,
; requiring unique handling.
;
; b. and c. procedures for sums, differences, products, and exponentials.
; Since we don't yet have "get-proc" and "put-proc" (until 3.3.3), we'll use a hack
; to do what we want. We'll ignore the table key 'deriv and just worry about
; the operator.
; (We're using "get-proc" and "put-proc", because "get" - at least - is already
; in scope, defined by Clojure.)

(def table (ref {}))
(defn get-proc [ignore op] 
  (get @table op))
  
(defn put-proc [key table-key proc]
  (let [new-map (assoc @table key proc)]
    (dosync (ref-set table new-map))))

(defn operator [exp] (first exp))
(defn operands [exp] (rest exp))
          
(defn deriv [exp variable]
  (cond (number? exp) 0
        (variable? exp)
          (if (same-variable? exp variable) 1 0)
        :else ((get-proc 'deriv (operator exp)) (operands exp) variable)))

(defn attach-tag [type-tag contents]
  (cons type-tag contents))
(defn type-tag [datum]
  (if (vector? datum)
      (first datum)
      (throw (RuntimeException. (format "Bad tagged datum -- TYPE-TAG: %s" datum)))))
(defn contents [datum]
  (if (vector? datum)
      (rest datum)
      (throw (RuntimeException. (format "Bad tagged datum -- CONTENTS: %s" datum)))))
      
(defn install-deriv-package []
  (defn sum [operands variable] 
    (make-sum (deriv (addend operands) variable)
              (deriv (augend operands) variable)))
  (defn difference [operands variable] 
    (make-difference (deriv (minuend operands) variable)
                     (deriv (subtrahend operands) variable)))
  (defn product [operands variable]
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) variable))
      (make-product (deriv (multiplier operands) variable)
                    (multiplicand operands))))
  (defn exponentiation [operands variable]
    (make-product
      (make-product (exponent operands)
                    (make-exponentiation (base operands) 
                                         (make-difference (exponent operands) 1)))
      (make-product (deriv (base operands) variable)
                    1)))

  (defn tag [x] (attach-tag 'deriv x))
  (put-proc '+  'deriv sum)
  (put-proc '-  'deriv difference)
  (put-proc '*  'deriv product)
  (put-proc '** 'deriv exponentiation)
  (put-proc :+  'deriv sum)
  (put-proc :-  'deriv difference)
  (put-proc :*  'deriv product)
  (put-proc :** 'deriv exponentiation)
  (put-proc "+"  'deriv sum)
  (put-proc "-"  'deriv difference)
  (put-proc "*"  'deriv product)
  (put-proc "**" 'deriv exponentiation)
  'done)
(install-deriv-package)

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
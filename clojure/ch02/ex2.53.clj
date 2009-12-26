(ns sicp.ch02 (:use clojure.test))

(defn memq [item x]
  (cond (empty? x) false
        (= item (first x)) x
        :else (memq item (rest x))))

(list 'a 'b 'c)                         ; => (a b c)
(list (list 'george))                   ; => ((george))
(rest '((x1 x2) (y1 y2)))               ; => ((y1 y2))
(nth  '((x1 x2) (y1 y2)) 1)             ; => (y1 y2)
(list? (first '(a short list)))         ; => false
(memq 'red '((red shoes) (blue socks))) ; => false
(memq 'red '(red shoes blue socks))     ; => (red shoes blue socks)

(deftest test-memq          
  (is (= '(a b c) (list 'a 'b 'c)))
  (is (= '((george)) (list (list 'george))))
  (is (= '((y1 y2)) (rest '((x1 x2) (y1 y2)))))
  (is (= '(y1 y2) (nth '((x1 x2) (y1 y2)) 1)))
  (is (= false (list? (first '(a short list)))))
  (is (= false (memq 'red '((red shoes) (blue socks)))))
  (is (= '(red shoes blue socks) (memq 'red '(red shoes blue socks)))))

(run-tests)
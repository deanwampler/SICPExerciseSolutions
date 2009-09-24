; No tests...
(defn f [g] (g 2))

(defn square [n] (* n n))

(f square)
(f #(* % (+ % 1)))

(f f)
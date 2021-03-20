(set-logic LIA)

(synth-fun f ((x Int) Int))

(constraint (= (f 1) 0))
(constraint (= (f 2) 1))
(constraint (= (f 3) 2))
(constraint (= (f 4) 0))
(constraint (= (f 5) 1))
(constraint (= (f 6) 2))
(constraint (= (f 7) 0))
(constraint (= (f 8) 1))
(constraint (= (f 9) 2))


(check-synth)
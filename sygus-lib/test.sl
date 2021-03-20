(set-logic LIA)

(synth-fun f ((x Int) (y Int)) Int
    ((Start Int (x y 0 1 2))))

(declare-var x Int)
(declare-var y Int)
(constraint (= (f x y) (f y x)))
(constraint (> (f x y) 1))

(check-synth)
#lang rosette

(define (poly x l)
   (apply +
      (for/list ([i (in-range (length l))])
        (* (expt x i) (list-ref l i))
      )
   )
)

(define-symbolic x integer?)
(define l (list 1 2 3))
(define root
  (solve (begin (assert (= (poly x l) 0)))))
(poly x l)
(evaluate x root)

(define 
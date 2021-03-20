#lang rosette

(require 
  rosette/lib/match
  rosette/lib/synthax)

; Defines a grammar for boolean expressions
; in negation normal form (NNF).
(define-synthax (nnf x y depth)
 #:base (choose x y 0 1)
 #:else (choose
         x y 0 1
         ((choose + -) (nnf x y (- depth 1))
                         (nnf x y (- depth 1)))))
 

; The body of nnf=> is a hole to be filled with an
; expression of depth (up to) 1 from the NNF grammar.
(define (nnf=> x y)
  (nnf x y 1))


(define-symbolic a b integer?)
(print-forms
   (synthesize
    #:forall (list a b)
    #:guarantee (assert (equal? (+ a b) (nnf=> a b)))))
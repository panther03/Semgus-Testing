#lang rosette

(current-bitwidth #f)

(require 
  rosette/lib/match
  rosette/lib/destruct
  rosette/lib/angelic)

(define-symbolic x y integer?)

; Define node structures

; Start nonterminal

(struct assn_x (iexp) #:transparent)
(struct assn_y (iexp) #:transparent)

; E nonterminal
(struct plus_st (iexp1 iexp2) #:transparent)
(struct varx_st () #:transparent)
(struct vary_st () #:transparent)
(struct const1_st () #:transparent)
(struct const0_st () #:transparent)

; B nonterminal
(struct true_st () #:transparent)
(struct false_st () #:transparent)
(struct not_st (bexp) #:transparent)
(struct and_st (bexp1 bexp2) #:transparent)
(struct lt_st (iexp1 iexp2) #:transparent)
(struct gt_st (iexp1 iexp2) #:transparent)

; Output type rx, ry (int, int)
(define (Start.Sem p x y)
  (destruct p
    [(assn_x iexp) (list (E.Sem iexp x y) y)]
    [(assn_y iexp) (list x (E.Sem iexp x y))]
    [_ p]))

; output type: re (int)
(define (E.Sem p x y)
  (destruct p
    [(plus_st iexp1 iexp2) (+ (E.Sem iexp1 x y) (E.Sem iexp2 x y))]
    [(varx_st) x]
    [(vary_st) y]
    [(const1_st) 1]
    [(const0_st) 0]
    [_ p]))

(define (B.Sem p x y)
  (destruct p
    [(lt_st iexp1 iexp2) (< (E.Sem iexp1 x y) (E.Sem iexp2 x y))]
    [(gt_st iexp1 iexp2) (> (E.Sem iexp1 x y) (E.Sem iexp2 x y))]
    [(true_st) #t]
    [(false_st) #f]
    [(not_st bexp) (! (B.Sem bexp x y))]
    [(and_st bexp1 bexp2) (&& (B.Sem bexp1 x y) (B.Sem bexp2 x y))]
    [_ p]))
 


; we assume depth > 0, there are no terminals for this LHS

; replace with define-grammar

(define (Start depth)
  (choose* (assn_x (E (- depth 1))) (assn_y (E (- depth 1)))))
  
(define (E depth)
  (if (= depth 0)
      (choose* (varx_st) (vary_st) (const1_st) (const0_st))
      (choose* (varx_st) (vary_st) (const1_st) (const0_st)
               (plus_st (E (- depth 1)) (E (- depth 1)))
               )))

(define (B depth)
  (if (= depth 0)
      (choose* (true_st) (false_st))
      (choose* (true_st) (false_st)
               (lt_st (E (- depth 1)) (E (- depth 1)))
               (gt_st (E (- depth 1)) (E (- depth 1)))
               (and_st (B (- depth 1)) (B (- depth 1)))
               (not_st (B (- depth 1)))
               )))

; Test E.Sem
; (E.Sem (plus_st (varx_st) (plus_st (vary_st) (const1_st))) 3 4)

; Define function to generate
(define max2_start
  (Start 4))

(define max2_b
  (B 3))

; Try to generate x=x+y+1  
(define sol_start
  (synthesize
   #:forall (list)
   #:guarantee  (assert (&& (equal? (Start.Sem max2_start 0 1) (list 2 1)) (equal? (Start.Sem max2_start 2 2) (list 5 2))))))


; Try to generate x == b <=> !(x < y) && !(x > y)
(define sol_b
  (synthesize
   #:forall (list)
   #:guarantee  (assert (&& (equal? (B.Sem max2_b -1 1) #t) (equal? (B.Sem max2_b 2 -1) #f) (equal? (B.Sem max2_b 1 0) #f)))))
; Boolean literals are not in the semgus format (the constraints use integers). Is it reasonabe to use them here?
; Possible to synthesize "x < y", "x > y", or "x == y" with just example based constraints?

(evaluate max2_start sol_start)
(evaluate max2_b sol_b)
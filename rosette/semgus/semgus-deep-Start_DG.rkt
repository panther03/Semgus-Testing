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

(define-grammar (gram)
  [Start
   (choose* (assn_x (E (- depth 1))) (assn_y (E (- depth 1))))]
  [E
   (choose* (varx_st) (vary_st) (const1_st) (const0_st)
               (plus_st (E (- depth 1)) (E (- depth 1))))]
  [B
   (choose* (true_st) (false_st)
               (lt_st (E (- depth 1)) (E (- depth 1)))
               (gt_st (E (- depth 1)) (E (- depth 1)))
               (and_st (B (- depth 1)) (B (- depth 1)))
               (not_st (B (- depth 1))))])

; Test E.Sem
; (E.Sem (plus_st (varx_st) (plus_st (vary_st) (const1_st))) 3 4)

; Define function to generate
(define (f)
  (gram #:depth 3))

; Try to generate x=x+y+1  
(define sol
  (synthesize
   #:forall (list)
   #:guarantee  (assert (&& (equal? (Start.Sem f 0 1) (list 2 1)) (equal? (Start.Sem f 2 2) (list 5 2))))))

(evaluate f sol)
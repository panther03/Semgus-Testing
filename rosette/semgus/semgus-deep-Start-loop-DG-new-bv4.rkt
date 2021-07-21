#lang rosette/safe

(require
  rosette/lib/match
  rosette/lib/destruct
  rosette/lib/angelic
  rosette/lib/synthax)

(current-bitwidth 4)

; Define node structures

; Start nonterminal
(struct assn_x (iexp) #:transparent)
(struct assn_y (iexp) #:transparent)
(struct ite_st (cond stmt1 stmt2) #:transparent)
(struct while_st (cond stmt) #:transparent)
(struct semi_st (stmt1 stmt2) #:transparent)
; E nonterminal
(struct plus_st (iexp1 iexp2) #:transparent)
(struct sub_st (iexp1 iexp2) #:transparent)
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

(define bvone  (bv #b1 4))
(define bvzero  (bv #b0 4))

; Helper function for Start.Sem (removes c from state)
(define (Start.Semantics p x y c)
  (let ([thr (Start.Sem p x y c)]) (list (first thr) (second thr))))

; Output type rx, ry, rc (int, int)
(define (Start.Sem p x y c)
  (assert (bvsge c bvzero))
  (destruct p
    [(assn_x iexp) (list (E.Sem iexp x y) y (bvsub1 c))]
    [(assn_y iexp) (list x (E.Sem iexp x y) (bvsub1 c))]
    [(ite_st cond stmt1 stmt2)
               (if (B.Sem cond x y)
                    (Start.Sem stmt1 x y (bvsub1 c))
                    (Start.Sem stmt2 x y (bvsub1 c))
               )
    ]
    [(while_st cond stmt)   (if (B.Sem cond x y)
                                (let ([stmt_sem (Start.Sem stmt x y (bvsub1 c))])
                                    (Start.Sem p (first stmt_sem) (second stmt_sem) (bvsub1 (third stmt_sem))))
                                (list x y c))]
    [(semi_st stmt1 stmt2) (let ([stmt1_sem (Start.Sem stmt1 x y (bvsub1 c))])  (Start.Sem stmt2 (first stmt1_sem) (second stmt1_sem) (bvsub1 (third stmt1_sem))))]
    ))


; output type: re (int)
(define (E.Sem p xe ye)
  (destruct p
    [(plus_st iexp1 iexp2) (bvadd (E.Sem iexp1 xe ye) (E.Sem iexp2 xe ye))]
    [(sub_st iexp1 iexp2) (bvsub (E.Sem iexp1 xe ye) (E.Sem iexp2 xe ye))]
    [(vary_st) ye]
    [(varx_st) xe]
    [(const1_st) bvone]
    [(const0_st) bvzero]
    ))
; output type: rb (bool)
(define (B.Sem b xb yb)
  (destruct b
    [(lt_st iexp1 iexp2) (bvslt (E.Sem iexp1 xb yb) (E.Sem iexp2 xb yb))]
    [(gt_st iexp1 iexp2) (bvsgt (E.Sem iexp1 xb yb) (E.Sem iexp2 xb yb))]
    [(true_st) bvone]
    [(false_st) bvzero]
    [(not_st bexp) (bvnot (B.Sem bexp xb yb))]
    [(and_st bexp1 bexp2) (bvand (B.Sem bexp1 xb yb) (B.Sem bexp2 xb yb))]
    ))

(current-grammar-depth 4)
(define-grammar (gram)
  [Start
   (choose
    (while_st (B) (Start))
    (assn_x (E))
    (assn_y (E))
    (ite_st (B) (Start) (Start))
    (semi_st (Start) (Start))
    )]
  [E
   (choose
           (varx_st)
           (vary_st)
           (const1_st)
           (const0_st)
           (plus_st (E) (E))
           (sub_st (E) (E))
           )
           ]
  [B
   (choose (true_st)
           (false_st)
           (lt_st (E) (E))
           (gt_st (E) (E))
           (and_st (B) (B))
           (not_st (B))
           )
   ]
  )

; Define function to generate
(define (sol)
  (gram #:depth 4))

;(Start.Semantics (while_st (gt_st (const0_st) (vary_st)) (assn_x (plus_st (varx_st) (const1_st)))) (bv 1 8) (bv 3 8) (bv 7 8))

; Try to generate while x<y; x = x+1; y = y-1;
(define sol_start
  (synthesize
   #:forall (list)
   #:guarantee  (assert
                  (equal? (Start.Semantics (sol) (bv 1 4) (bv 3 4) (bv 3 4)) (list (bv #x3 4) (bv #x3 4)))
                 )))

(print-forms sol_start)


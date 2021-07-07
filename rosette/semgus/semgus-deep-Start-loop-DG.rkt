#lang rosette

(current-bitwidth #f)

(require 
  rosette/lib/match
  rosette/lib/destruct
  rosette/lib/angelic
  rosette/lib/synthax)

(define-symbolic x y c integer?)

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

; Output type rx, ry, rc (int, int)
(define (Start.Sem p x y c)
  (destruct p
    [(assn_x iexp) (list (E.Sem iexp x y) y c)]
    [(assn_y iexp) (list x (E.Sem iexp x y) c)]
    [(ite_st cond stmt1 stmt2) (if (B.Sem cond x y) (Start.Sem stmt1 x y c) (Start.Sem stmt2 x y c))]
    [(while_st cond stmt)   (if (B.Sem cond x y) 
                                (let ([stmt_sem (Start.Sem stmt x y c)])
                                  (Start.Sem p (first stmt_sem) (second stmt_sem) (- c 1)))
                                (list x y c))]
    [(semi_st stmt1 stmt2) (let ([stmt1_res (Start.Sem stmt1 x y c)]) (Start.Sem stmt2 (first stmt1_res) (second stmt1_res) (third stmt1_res)))]
    [_ p]))


; output type: re (int)
(define (E.Sem p x y)
  (destruct p
    [(plus_st iexp1 iexp2) (+ (E.Sem iexp1 x y) (E.Sem iexp2 x y))]
    [(sub_st iexp1 iexp2) (- (E.Sem iexp1 x y) (E.Sem iexp2 x y))]
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
; have to bound depth, but is that applicable here?

(current-grammar-depth 4)
(define-grammar (gram)
  [Start
   (choose (assn_x (E)) (assn_y (E))
        (ite_st (B) (Start) (Start))
        (while_st (B) (Start)) 
        (semi_st (Start) (Start)))]
  [E
   (choose (varx_st) (vary_st) (const1_st) (const0_st)
               (plus_st (E) (E))
               (sub_st (E) (E)))]
  [B
   (choose (true_st) (false_st)
               (lt_st (E) (E))
               (gt_st (E) (E))
               (and_st (B) (B))
               (not_st (B)))])

; Define function to generate
(define (f)
  (gram #:depth 5))

(define test_prog
    (while_st (lt_st (varx_st) (vary_st)) (semi_st (assn_x (plus_st (varx_st) (const1_st))) (assn_y (sub_st (vary_st) (const1_st))))))

; Try to generate while x<y; x = x+1; y = y-1;
(define sol_start
  (synthesize
   #:forall (list)
   #:guarantee  (assert (equal? (Start.Sem (f) 0 0 6) (list 0 0 6)))
                            ))


;(equal? (Start.Sem max2_start 0 7) (list 4 3))
                         ;   (equal? (Start.Sem max2_start 5 2) (list 5 2))
;(Start.Sem test_prog 0 7 6)
(print-forms sol_start)

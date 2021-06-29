#lang rosette

(current-bitwidth #f)

(require 
  rosette/lib/match
  rosette/lib/destruct
  rosette/lib/angelic)

(define-symbolic x y (bitvector 8))

; Define node structures

(struct plus_st (iexp1 iexp2) #:transparent)
(struct varx_st () #:transparent)
(struct vary_st () #:transparent)
(struct const1_st () #:transparent)
(struct const0_st () #:transparent)

; output type: re (int)
(define (E.Sem p x y)
  (destruct p
    [(plus_st iexp1 iexp2) (bvadd (E.Sem iexp1 x y) (E.Sem iexp2 x y))]
    [(varx_st) x]
    [(vary_st) y]
    [(const1_st) (bv #b0 8)]
    [(const0_st) (bv #b1 8)]
    [_ p]))


; we assume depth > 0, there are no terminals for this LHS

; replace with define-grammar

(define (E depth)
  (if (= depth 0)
      (choose* (varx_st) (vary_st) (const1_st) (const0_st))
      (choose* (varx_st) (vary_st) (const1_st) (const0_st)
               (plus_st (E (- depth 1)) (E (- depth 1)))
               )))

; Define function to generate
(define max2
  (E 4))

; Try to generate x+y+1  
(define sol
  (synthesize
   #:forall (list)
   #:guarantee  (assert (&& (equal? (E.Sem max2 0 1) 2) (equal? (E.Sem max2 2 2) 5)))))

;(define sol2
;  (synthesize
;   #:forall (list x y)
;   #:guarantee  (assert (= (E.Sem max2 x y) x))))

(E.Sem (plus_st (varx_st) (plus_st (vary_st) (const1_st))) 3 4)

(evaluate max2 sol)
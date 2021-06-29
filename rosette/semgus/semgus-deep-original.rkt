#lang rosette

(current-bitwidth #f)

(require 
  rosette/lib/match
  rosette/lib/destruct
  rosette/lib/angelic)

(define-symbolic x y c integer?)

; Define node structures

(struct assn_x (iexp) #:transparent)
(struct assn_y (iexp) #:transparent)
(struct assn_c (iexp) #:transparent)
(struct site (bexp s1 s2) #:transparent)
(struct sitee (bexp e1 e2) #:transparent)
(struct not_st (bexp) #:transparent)
(struct and_st (bexp1 bexp2) #:transparent)
(struct plus_st (iexp1 iexp2) #:transparent)
(struct lt_st (iexp1 iexp2) #:transparent)

; Semantics of the grammar
; output format: (rx, ry, rc)
(define (Start.Sem p x y c)
  (destruct p
    [(assn_x iexp) (list (E.Sem iexp x y c) y c)]
    [(assn_y iexp) (list x (E.Sem iexp x y c) c)]
    [(assn_c iexp) (list x y (E.Sem iexp x y c))]
    ;[(site bexp s1 s2) (if (B.Sem bexp x y c) (Start.Sem s1 x y c) (Start.Sem s2 x y c))]
    [_ p]))

; output format: re
(define (E.Sem p x y c)
  (destruct p
    [(plus_st iexp1 iexp2) (+ (E.Sem iexp1 x y c) (E.Sem iexp2 x y c))]
   ; [(sitee bexp e1 e2) (if (B.Sem bexp x y c) (E.Sem e1 x y c) (E.Sem e2 x y c))]
    [_ p]))

; output format rb
(define (B.Sem p x y c)
  (destruct p
    [(not_st bexp) (! (B.Sem bexp x y c))]
    [(and_st bexp1 bexp2) (&& (B.Sem bexp1 x y c) (B.Sem bexp2 x y c))]
    [(lt_st iexp1 iexp2) (<= (E.Sem iexp1 x y c) (E.Sem iexp2 x y c))]
    [_ p]))

; we assume depth > 0, there are no terminals for this LHS

(define (Start depth)
  (if (= depth 0)
      (choose*
               (assn_x (E (- depth 1)))
               (assn_y (E (- depth 1)))
               (assn_c (E (- depth 1)))
               (site (B (- depth 1)) (Start (- depth 1)) (Start (- depth 1)))
               )))

(define (E depth)
  (if (= depth 0)
      (choose* 0 2 x y)
      (choose* 0 2 x y
               (plus_st (E (- depth 1)) (E (- depth 1)))
               ;(sitee (B (- depth 1)) (I (- depth 1)) (I (- depth 1)))
               )))

(define (B depth)
  (if (= depth 0)
      (choose* #t #f)
      (choose* #t #f
               (not_st (B (- depth 1)))
               (and_st (B (- depth 1)) (B (- depth 1)))
               (lt_st (E (- depth 1)) (E (- depth 1))))))

; Define function to generate
(define max2
  (Start 3))

; Constraints for problem given
(define p (assn_x (plus_st x y)))
(define q (assn_x (sitee (lt_st x y) y x))) ; reference program using expression-based ITE
(define r (site (lt_st x y) (assn_x y) (assn_x x))) ; reference program using statement-based ITE

(define (sol f)
    (synthesize
   #:forall (list x y c)
    #:guarantee (f max2)))

; Try to generate simple x+y program (works when ITE semantic & syntax rules are commented out)

(define (constraints_p prog)
  (assert (= (first (Start.Sem prog x y c)) (first (Start.Sem p x y c)))))
(evaluate max2 (sol constraints_p))

; Try to generate using expression-based ITE (runs forever)
(define (constraints_q prog)
  (assert (= (first (Start.Sem prog x y c)) (first (Start.Sem q x y c)))))
;(evaluate max2 (sol constraints_q))

; Try to generate using statement-based ITE (also runs forever)
(define (constraints_r prog)
  (assert (= (first (Start.Sem prog x y c)) (first (Start.Sem r x y c)))))
;(evaluate max2 (sol constraints_r))



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
    [(site bexp s1 s2) (let ([rr1 (Start.Sem s1 x y c)]
                             [rr2 (Start.Sem s2 x y c)]
                             [bb (B.Sem bexp x y c)])
                         (list (if bb (first rr1) (first rr2))
                           (if bb (second rr1) (second rr2))
                           (if bb (third rr1) (third rr2))))]
    [_ p]))

; output format: re
(define (E.Sem p x y c)
  (destruct p
    [(plus_st iexp1 iexp2) (+ (E.Sem iexp1 x y c) (E.Sem iexp2 x y c))]
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
      (choose*
               (assn_x (I (- depth 1)))
               (assn_y (I (- depth 1)))
               (assn_c (I (- depth 1)))
               (site (B (- depth 1)) (I (- depth 1)) (I (- depth 1)))))

(define (I depth)
  (if (= depth 0)
      (choose* 0 2 x y)
      (choose* 0 2 x y
               (plus_st (I (- depth 1)) (I (- depth 1))))))

(define (B depth)
  (if (= depth 0)
      (choose* #t #f)
      (choose* #t #f
               (not_st (B (- depth 1)))
               (and_st (B (- depth 1)) (B (- depth 1)))
               (lt_st (I (- depth 1)) (I (- depth 1))))))

; Define function to generate
(define max2
  (Start 2))

; Constraints for problem given
(define p (assn_x (plus_st x y)))
(define (constraints prog)
  (assert (= (first (Start.Sem prog x y c)) (first (Start.Sem p x y c)))))

; Synthesize the function
(define sol
    (synthesize
   #:forall (list x y c)
    #:guarantee (constraints max2)))

; Synthesize an example program

; Interpret (execute)
(define q (site (lt_st x y) (assn_x y) (assn_x x)))
(Start.Sem q x y c)

; Run max2 against the bindings found by the synthesizer to generate the program form
(evaluate max2 sol)


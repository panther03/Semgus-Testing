#lang rosette

(require rosette/lib/synthax)

(provide Not And Or Xor Iff ver syn ??inst)

(define (Not a)   (if a #f #t))
(define (And a b) (if a b #f))
(define (Or a b)  (if a #t b))
(define (Iff a b) (if a b (! b)))
(define (Xor a b) (if a (! b) b))

(define (Niffp a b c d)
  (define r1 (Iff a b))
  (define r2 (Iff c d))
  (define r3 (Iff r1 r2))
  (define r4 (Not r3))
  r4)

(define (NXp a b c d)
  (define r1 (Xor a b))
  (define r2 (Not r1))
  (define r3 (Xor c d))
  (define r4 (Not r3))
  (define r5 (Xor r2 r4))
  r5)

(define (ver impl spec)
  (define-symbolic* in boolean? [(procedure-arity impl)])
  (define sol
    (verify (assert (equal? (apply impl in) (apply spec in)))))
  (and (sat? sol) (evaluate in sol)))

(define-synthax ??inst
  ([[_ arg ...]
    (choose (Not (choose arg ...))
            ((choose And Or Iff Xor)
             (choose arg ...) (choose arg ...)))]))

(define (syn impl spec)
  (define-symbolic* in boolean? [(procedure-arity impl)])
  (define syn
    (synthesize
   #:forall in
   #:guarantee (assert (equal? (apply impl in) (apply spec in)))))
  (and (sat? syn) (generate-forms syn)))

(define (AIGp a b c d)
  (define r1 (Not a))
  (define r2 (Not b))
  (define r3 (And r1 r2))
  (define r4 (Not r3))
  (define r5 (And a b))
  (define r6 (Not r5))
  (define r7 (And r4 r6))
  (define r8 (Not r7))
  (define r9 (Not c))
  (define r10 (Not d))
  (define r11 (And r9 r10))
  (define r12 (Not r11))
  (define r13 (And c d))
  (define r14 (Not r13))
  (define r15 (And r12 r14))
  (define r16 (Not r15))
  (define r17 (Not r8))
  (define r18 (Not r16))
  (define r19 (And r17 r18))
  (define r20 (Not r19))
  (define r21 (And r8 r16))
  (define r22 (Not r21))
  (define r23 (And r20 r22))
  (define r24 (Not r23))
  (define r25 (Not r24))
  r25)

(define (AIGp_impl a b c d)
   (??inst a b c d))

(define (sk-1 a b c d)
  (define r1 (??inst a b c d))
  r1)

(define (sk-2 a b c d)
  (define r1 (??inst a b c d))
  (define r2 (??inst a b c d r1))
  r2)

(define (sk-3 a b c d)
  (define r1 (??inst a b c d))
  (define r2 (??inst a b c d r1))
  (define r3 (??inst a b c d r1 r2))
  r3)

; (syn sk-1 AIGp)
; (syn sk-2 AIGp)
; (syn sk-3 AIGp)
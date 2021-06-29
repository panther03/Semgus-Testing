#lang rosette

(require 
  rosette/lib/match
  rosette/lib/synthax
  (prefix-in s (only-in "shallow.rkt" Not And Or Iff Xor)))

(struct Not (a)   #:transparent)
(struct And (a b) #:transparent)
(struct Or  (a b) #:transparent)
(struct Iff (a b) #:transparent)
(struct Xor (a b) #:transparent)

(define (interpret prog in)
  (define reg (make-vector (+ (car prog) (length (cdr prog))) #f))
  (define (store i v) (vector-set! reg i v))
  (define (load i) (vector-ref reg i))
  (define (last) (sub1 (vector-length reg)))
  (for ([(i idx) (in-indexed in)])
    (store idx i))
  (for ([(inst idx) (in-indexed (cdr prog))])
    (store
     (+ idx (car prog)) 
     (match inst
       [(Not a)   (sNot (load a))]
       [(And a b) (sAnd (load a) (load b))]
       [(Or a b)  (sOr (load a) (load b))]
       [(Xor a b) (sXor (load a) (load b))]
       [(Iff a b) (sIff (load a) (load b))])))
  (load (last)))

(define (ver impl spec)
  (define-symbolic* in boolean? [(car impl)])
  (define sol
    (verify (assert (equal? (interpret impl in) (interpret spec in)))))
  (and (sat? sol) (evaluate in sol)))

(define NXp
  (list 4 (Xor 0 1) (Not 4) (Xor 2 3) (Not 6) (Xor 5 7)))

(define Niffp
   (list 4 (Xor 0 1) (Not 4) (Xor 2 3) (Not 6) (Xor 5 7)))

(ver NXp Niffp)

(define sketch
(case-lambda
  [(n k) (sketch n k Not And Or Xor Iff)]
  [(n k . insts) (cons n (build-list k (lambda (x) (apply choose insts))))]))

(define (syn impl spec)
  (define-symbolic* in boolean? [(car spec)])
  (define syn
    (synthesize
   #:forall in
   #:guarantee (assert (equal? (interpret impl in) (interpret spec in)))))
  (and (sat? syn) (evaluate syn)))

(define (superopt spec)
  (define-symbolic len integer?)
  (solve (assert ((define out (syn (sketch (car spec) len) spec)) out))))
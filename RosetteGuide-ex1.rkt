#lang rosette

(require rosette/query/debug rosette/lib/render)
(require rosette/lib/synthax)

(define (abs-spec x)
  (if (bvslt x (bv 0 32))
      (bvneg x)
      x))

(define/debug (abs-impl x) 
  (let* ([o1 (bvashr x (bv 31 32))]
         [o2 (bvadd x o1)]
         [o3 (bvsub o2 o1)])
    o3))

(define (abs-impl-fixed x)
  (let* ([o1 (bvashr x (bv 31 32))]
         [o2 ((choose bvadd bvand bvor bvxor bvshl bvlshr bvashr) x o1)]
         [o3 ((choose bvadd bvand bvor bvxor bvshl bvlshr bvashr) o2 o1)])
    o3))

; Verify that the code works on all inputs or find a counterexample
(define-symbolic y (bitvector 32))
(verify (assert (equal? (abs-spec y) (abs-impl y))))

; Debug the code against the found input

(define b (bv #x8404c7c0 32))

(render
 (debug [(bitvector 32)]
        (assert (equal? (abs-spec b) (abs-impl b)))))

; Synthesize a fixed version


(define (abs-impl-3 x) 
  (let* ([o1 (bvashr x (bv 31 32))]
         [o2 ((choose bvadd bvand bvor bvxor bvshl bvlshr bvashr) x o1)]
         [o3 ((choose bvsub bvand bvor bvxor bvshl bvlshr bvashr) o2 o1)])
    o3))

(define-symbolic i (bitvector 32))

(print-forms
   (synthesize
    #:forall i
    #:guarantee (assert (equal? (abs-spec i) (abs-impl-fixed i)))))


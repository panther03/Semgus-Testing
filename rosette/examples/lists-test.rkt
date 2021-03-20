#lang rosette

(define k 32) ; upper bound
(define-symbolic* v integer? [k]) ; list of k integers
(define-symbolic n integer?) ; unknown length
(define vn (take v n)) ; list of up to k integers

(define (sumz l)
  (= (foldl + 0 l) -32))

(solve (assert (sumz vn)))

#lang rosette

(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/destruct)

(define x 0)
(define y 0)

; While stmt
(struct while (cond stmt) #:transparent) ; node #1

; StmtNodes
(struct semicolon (stmt1 stmt2) #:transparent) ; node #2 
(struct assign_x (exp) #:transparent)
(struct assign_y (exp) #:transparent)

; BoolExpNodes
(struct lt_exp (exp1 exp2) #:transparent)

; ExpNodes
(struct and_node (exp1 exp2) #:transparent)
(struct or_node (exp1 exp2) #:transparent)
(struct plus_node (exp1 exp2) #:transparent)

(struct x_node () #:transparent)
(struct y_node () #:transparent)
(struct const (i) #:transparent)

; explicit state ?
; state : map ( operations: update state, get/read) (variables -> values)
; Program semantics
(define (interpret p)
  (destruct p
    [(while cond stmt)  (when (interpret cond) (begin (interpret stmt) (interpret (while cond stmt))))]
    [(semicolon stmt1 stmt2) (begin (interpret stmt1) (interpret stmt2))]
    [(assign_x exp) (set! x (interpret exp))]
    [(assign_y exp) (set! y (interpret exp))]
    [(lt_exp exp1 exp2) (< (interpret exp1) (interpret exp2))]
    [(and_node exp1 exp2) (bitwise-and (interpret exp1) (interpret exp2))]
    [(or_node exp1 exp2) (bitwise-ior (interpret exp1) (interpret exp2))]
    [(plus_node exp1 exp2) (+ (interpret exp1) (interpret exp2))]
    [(const i) i]
    [(x_node) x]
    [(y_node) y]
    [_ p]))

;(define (interpret_list p)
;  (match p
;    [(list* 1 l) (begin (when (interpret_list l) (begin (interpret_list (interpret_list l)) (interpret_list (interpret_list (interpret_list l))))) l)]
;    [(list* 2 l) (begin (< (interpret_list (interpret_list l)) (interpret_list l)) l)]
;    [(list* 6 l) 
;    ))

; Map a node number to the number of required child nodes
(define nodes-map '(2 2 2 1 1 0 0 2 2))

; Take a list l of integers (pre-order traversal) and treat it as a traversed tree
; output the tree which generated that traversal
; Use a pair (l, t) where l is the remainder of the list that has not been traversed,
; and t is the output tree

; Skip tree entirely and generate semantics from list representation?

;(define (gen-tree-helper l ss x)
;  (match x
;    [0 (cons l ss)]
;    [x (let ([p (gen-tree l)]) (gen-tree-helper (car p 0) ( (- x 1)))]
;  )
;)

;(define (gen-tree l)
;  (list-ref nodes-map (first l))
;  ; todo: need to mutate l?
;  ; ss := []
;    (let*  ([l_temp '()] [ss '()])
;      (list-ref nodes-map head l)
;      (for ([i (in-range ))])
;        (begin (append (car (gen-tree (gen-tree (tail l)))))
;               (
;      )
;   )
; )

  ; 
  ; for i := 0; i < nodes-map[head l]; i++ {
  ;    ss += (gen-tree (gen-tree (tail l))[0])[1]
  ; }
  ; todo: how to map a node type m to a struct def programmatically?
  ; return (l, (struct Structs[m] ss))
  ;)

(define k 32) ; upper bound
(define-symbolic* v integer? [k]) ; list of k integers
(define-symbolic n integer?) ; unknown length
(define vn (take v n)) ; list of up to k integers

;(interpret (semicolon (assign_x (const 3)) (assign_y (plus_node (x_node) (const 4)))))

; y := 5; while (x < y) x := x + 1;
(interpret
 (semicolon (assign_y (const 5))
            (while (lt_exp (x_node) (y_node)) (assign_x (plus_node (x_node) (const 1))))))
;(define prog
;  (synthesize
;   #:guarantee (assert (eq? (begin (interpret prog) y = 5)))))
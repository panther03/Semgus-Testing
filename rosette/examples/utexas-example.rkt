#lang rosette/safe


(require rosette/lib/angelic    ; provides `choose*`
         rosette/lib/destruct)  ; provides `destruct`
; Tell Rosette we really do want to use integers.
(current-bitwidth #f)



; Compute the absolute value of `x`.
(define (absv x)
  (if (< x 0) (- x) x))

; Define a symbolic variable called y of type integer.
(define-symbolic y integer?)

; Solve a constraint saying |y| = 5.
(solve
  (assert (= (absv y) 5)))


; Try to outsmart Rosette by asking for the impossible:
(solve (assert (< (absv y) 0)))


; Syntax for our simple DSL
(struct plus (left right) #:transparent)
(struct mul (left right) #:transparent)
(struct square (arg) #:transparent)


; A simple program
(define prog (plus (square 7) 3))


; Interpreter for our DSL.
; We just recurse on the program's syntax using pattern matching.
(define (interpret p)
  (destruct p
    [(plus a b)  (+ (interpret a) (interpret b))]
    [(mul a b)   (* (interpret a) (interpret b))]
    [(square a)  (expt (interpret a) 2)]
    [_ p]))


; (plus (square 7) 3) evaluates to 52.
(interpret prog)


; Our interpreter works on symbolic values, too.
(interpret (square (plus y 2)))


; So we can search for a `y` that makes (y+2)^2 = 25
(solve 
  (assert 
    (= (interpret (square (plus y 2))) 25)))


; Find values for `x` and `c` such that c*x = x+x.
; This is our first synthesis attempt, but it doesn't do what we want,
; which is to find a `c` that works for *every* x.
(define-symbolic x c integer?)
(solve 
  (assert 
    (= (interpret (mul c x)) (+ x x))))


; Find a `c` such that c*x = x+x for *every* x.
(synthesize
  #:forall (list x)
  #:guarantee (assert (= (interpret (mul c x)) (+ x x))))


; Create an unknown expression -- one that can evaluate to several
; possible values.
(define (??expr terminals depth)
  (if (= depth 0) (apply choose* terminals) (choose* (plus (??expr terminals (- depth 1)) (??expr terminals (- depth 1)))
                                                     (mul (??expr terminals (- depth 1)) (??expr terminals (- depth 1)))
                                                     (apply choose* terminals))))

(define (I a b depth)
  (if (= depth 0)
      (choose* a b 0 1)
      (choose* a b 0 1
               (plus (I a b (- depth 1)) (I a b (- depth 1))))))


; Create a sketch representing all programs of the form (plus ?? ??),
; where the ??s are unknown expressions created by ??expr.
(define-symbolic p q integer?)
(define sketch
  (I x y 3))


; Solve the sketch to find a program equivalent to 10*x,
; but of the form (plus ?? ??). Save the resulting model.
(define M
  (synthesize
    #:forall (list x y)
    #:guarantee (assert (= (interpret sketch) (+ y x)))))


; Substitute the bindings in M into the sketch to get back the
; synthesized program.
(evaluate sketch M)
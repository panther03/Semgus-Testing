#lang rosette

(define (choice type) (define-symbolic z type) z) ; <-- implement choice

(define (more-or-less-1 x)
  (if (choice boolean?)
      (+ x 1)
      (- x 1)))

(solve (assert (= 0 (more-or-less-1 1)))) ; #f
(solve (assert (= 2 (more-or-less-1 1)))) ; #t

; A puzzle is represented as a list of 81 digits,
; obtained by concatenating the rows of the puzzle,
; from first to last. This procedure takes as input 
; a puzzle and checks that it satisfies the Sudoku rules.
(define (sudoku-check puzzle)    
  (for ([digit puzzle])             ; all digits between 1 and 9
    (assert (and (<= 1 digit) (<= digit 9))))  
  (for ([row (rows puzzle)])        ; all different in a row
    (assert (apply distinct? row)))            
  (for ([column (columns puzzle)])  ; all different in a column
    (assert (apply distinct? column)))        
  (for ([region (regions puzzle)])  ; all different in a 3x3 region
    (assert (apply distinct? region))))        

(define (rows puzzle [n 9])   
  (if (null? puzzle) 
      null
      (cons (take puzzle n)
            (rows (drop puzzle n) n))))

(define (columns puzzle [n 9])
  (if (null? puzzle)
      (make-list n null)
      (map cons
           (take puzzle n)
           (columns (drop puzzle n) n))))

(define (regions puzzle)
  (define rows3 (curryr rows 3))
  (define cols3 (curryr columns 3))
  (map flatten
       (append-map
        append
        (cols3
         (append-map
          rows3
          (cols3
           (append-map rows3 (rows puzzle))))))))

(define (char->digit c)
  (- (char->integer c) (char->integer #\0)))
  
(define (string->puzzle p)
  (map char->digit (string->list p)))

(define (show puzzle)
  (pretty-print (rows puzzle)))

(define (solve-puzzle puzzle) )
(define (valid-puzzle? puzzle) ...)
(define (generate-puzzle) ...)

; Sample solved puzzle.
(define p0 (string->puzzle "693784512487512936125963874932651487568247391741398625319475268856129743274836159"))
; Sample unsolved puzzle where 0 represents unfilled cells.
(define p1 (string->puzzle "000000010400000000020000000000050604008000300001090000300400200050100000000807000"))

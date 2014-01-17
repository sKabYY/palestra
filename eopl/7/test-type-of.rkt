#lang eopl

(#%require "numbool.rkt")
(#%require "test-cases.rkt")

(define (begin-red) (display "\033[91m"))
(define (begin-green) (display "\033[92m"))
(define (end-color) (display "\033[0m"))

(define (interp-disp interp sources)
  (define (iter count sources)
    (if (null? sources)
      'done
      (let ((src (car sources)))
        (newline)
        (display count)(display ":")(newline)
        (display "Source:")(newline)
        (begin-green)
        (display src)(newline)
        (end-color)
        (display "Type:")(newline)
        (begin-red)
        (eopl:pretty-print (interp src))
        (end-color)
        (iter (+ count 1) (cdr sources)))))
  (iter 1 sources))

(interp-disp (lambda (src) (type-poly->string (type-infer src)))
             numbool-type-cases)

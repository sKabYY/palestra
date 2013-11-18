#lang eopl

(#%provide (all-defined))

(define (begin-red) (display "\033[91m"))
(define (begin-green) (display "\033[92m"))
(define (end-color) (display "\033[0m"))

(define (interp-disp interp sources)
  (define (iter count sources)
    (if (null? sources)
      'done
      (let ((src (car sources)))
        (display count)(display ":")(newline)
        (begin-green)
        (display src)(newline)
        (begin-red)
        (eopl:pretty-print (interp src))
        (end-color)
        (iter (+ count 1) (cdr sources)))))
  (iter 1 sources))

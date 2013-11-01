#lang eopl

(#%provide (all-defined))

(define (displayln o)
  (display o) (newline))

(define (run-disp . exps)
  (for-each displayln exps))

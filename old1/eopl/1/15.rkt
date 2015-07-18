#lang eopl

(#%require "lib.rkt")

(define (duple n x)
  (if (= n 0)
    '()
    (cons x (duple (- n 1) x))))

(run-disp
  (duple 2 3)
  (duple 4 '(ha ha))
  (duple 0 '(blah)))

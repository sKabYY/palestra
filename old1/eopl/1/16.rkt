#lang eopl

(#%require "lib.rkt")

(define (invert lst)
  (if (null? lst)
    '()
    (cons
      (list (cadar lst) (caar lst))
      (invert (cdr lst)))))

(displayln
  (invert '((a 1) (a 2) (1 b) (2 b))))

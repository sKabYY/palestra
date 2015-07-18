#lang eopl

(#%require "lib.rkt")

(define (list-set lst n x)
  (cond ((null? lst) '())
        ((= n 0) (cons x (cdr lst)))
        (else
          (cons
            (car lst)
            (list-set (cdr lst) (- n 1) x)))))

(run-disp
  (list-set '(a b c d) 2 '(1 2))
  (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3))

#lang eopl

(#%require "lib.rkt")

(define (product sos1 sos2)
  (define (product-one x sos)
    (map (lambda (s) (list x s)) sos))
  (if (null? sos1)
    '()
    (append
      (product-one (car sos1) sos2)
      (product (cdr sos1) sos2))))

(run-disp
  (product '(a b c) '(x y)))

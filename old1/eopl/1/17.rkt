#lang eopl

(#%require "lib.rkt")

(define (down lst)
  (if (null? lst)
    '()
    (cons
      (list (car lst))
      (down (cdr lst)))))

(run-disp
  (down '(1 2 3))
  (down '((a) (fine) (idea)))
  (down '(a (more (complicated)) object)))

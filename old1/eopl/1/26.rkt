#lang eopl

(#%require "lib.rkt")

(define (up lst)
  (if (null? lst)
    '()
    (let ((head (car lst))
          (newtail (up (cdr lst))))
      (if (list? head)
        (append head newtail)
        (cons head newtail)))))

(run-disp
  (up '((1 2) (3 4)))
  (up '((x (y)) z)))

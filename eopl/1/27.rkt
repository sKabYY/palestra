#lang eopl

(#%require "lib.rkt")

(define (flatten slist)
  (if (null? slist)
    '()
    (let ((head (car slist))
          (newtail (flatten (cdr slist))))
      (if (list? head)
        (append (flatten head) newtail)
        (cons head newtail)))))

(run-disp
  (flatten '(a b c))
  (flatten '((a) () (b ()) () (c)))
  (flatten '((a b) c (((d)) e)))
  (flatten '(a b (() (c)))))

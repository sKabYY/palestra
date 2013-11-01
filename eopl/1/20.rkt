#lang eopl

(#%require "lib.rkt")

(define (count-occurrences s slist)
  (if (null? slist)
    0
    (let ((head (car slist))
          (tail (cdr slist)))
      (+
        (if (list? head)
          (count-occurrences s head)
          (if (eqv? head s) 1 0))
        (count-occurrences s tail)))))

(run-disp
  (count-occurrences 'x '((f x) y (((x z) x))))
  (count-occurrences 'x '((f x) y (((x z) () x))))
  (count-occurrences 'w '((f x) y (((x z) x)))))

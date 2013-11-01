#lang eopl

(#%require "lib.rkt")

(define (the-same? s1 s2) (eqv? s1 s2))

(define (swapper s1 s2 slist)
  (define (alternative a)
    (cond ((the-same? a s1) s2)
          ((the-same? a s2) s1)
          (else a)))
  (if (null? slist)
    '()
    (let ((head (car slist))
          (tail (cdr slist)))
      (cons
        (if (list? head)
          (swapper s1 s2 head)
          (alternative head))
        (swapper s1 s2 tail)))))

(run-disp
  (swapper 'a 'd '(a b c d))
  (swapper 'a 'd '(a d () c d))
  (swapper 'x 'y '((x) y (z (x)))))

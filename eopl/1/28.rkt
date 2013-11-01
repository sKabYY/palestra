#lang eopl

(#%require "lib.rkt")

(define (merge loi1 loi2)
  (cond ((null? loi1) loi2)
        ((null? loi2) loi1)
        (else
          (let ((i1 (car loi1))
                (i2 (car loi2)))
            (cond
              ((< i1 i2)
               (cons i1 (merge (cdr loi1) loi2)))
              (else
                (cons i2 (merge loi1 (cdr loi2)))))))))

(run-disp
  (merge '(1 4) '(1 2 8))
  (merge '(35 62 81 90 91) '(3 83 85 90)))

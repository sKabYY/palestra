#lang racket

(provide ir-of-cps)
(define (ir-of-cps expr)
  (>> expr))

(define (>> expr renv)
  (match expr
    [(? symbol? s) `(temp ,s)]
    [(? number? n) `(const ,n)]
    [(? boolean? b) `(boolean ,b)]
    [`(func (,a) ,body) 'todo]
    [`(if ,e1 ,e2 ,e3)
     (>> e1)
     (eseq (seq (deflabel (new-label))))
     ]
    [`(-/k ,e1 ,e2 ,k)]
    [`(zero?/k ,e1 ,k)]
    [`((,e1 ,e2) ,k)]
    [`(,k ,e)]))

#lang racket

(provide ir-of-cps)
(define (ir-of-cps expr)
  (>> expr))

(define (>> expr renv)
  (match expr
    [(? symbol? s) ]
    [(? number? n) ]
    [(? boolean? b)]
    [`(func (,a) ,body)]
    [`(if ,e1 ,e2 ,e3)]
    [`(-/k ,e1 ,e2 ,k)]
    [`(zero?/k ,e1 ,k)]
    [`((,e1 ,e2) ,k)]
    [`(,k ,e)]))

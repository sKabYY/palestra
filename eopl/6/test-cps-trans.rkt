#lang eopl

(#%require "letrec.rkt")
(#%require "lib.rkt")
(#%require "test-cases.rkt")

(define test-cases
  (list

"42"
"+(1, 2)"
"-(1, 2)"
"*(2, 3)"

"(proc (x, y) +(x, y) 2 3)"

"letrec double (x) = if zero?(x) then 0 else +(2, (double -(x, 1)))
in (double 12)"

))

(define (p src)
  (let* ((cps-pgm (cps-translate src))
         (result0 (interp src))
         (result (value-of-program/k cps-pgm)))
    (eopl:pretty-print result0)
    (eopl:pretty-print result)
    (cps-program->list cps-pgm)))

(interp-disp p letrec-cases)

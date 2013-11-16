#lang eopl

(#%require "lib.rkt")
(#%require "nameless-letrec.rkt")
(#%require "test-cases.rkt")

(interp-disp interp letrec-cases)

(eopl:pretty-print
  (interp "let f = proc (a, b) +(a, b) in (f 1 2 3)"))
(eopl:pretty-print
  (interp "let f = proc (a, b) +(a, b) in (f 1)"))

#lang eopl

(#%require "threads.rkt")
(#%require "lib.rkt")
(#%require "test-cases.rkt")

(interp-disp
  (lambda (src) (interp 5 src))
  expval->value
  imprefs-cp-cases
  other-summary)

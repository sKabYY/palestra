#lang eopl

(#%require "imprefs-cp.rkt")
(#%require "lib.rkt")
(#%require "test-cases.rkt")

(interp-disp interp expval->value imprefs-cp-cases other-summary)

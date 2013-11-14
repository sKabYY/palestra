#lang eopl

(#%require "letrec.rkt")
(#%require "lib.rkt")
(#%require "test-cases.rkt")

(interp-disp interp
             (append
               let-cases
               proc-cases
               letrec-cases))

#lang eopl

(#%require "proc.rkt")
(#%require "lib.rkt")
(#%require "test-cases.rkt")

(interp-disp interp (append let-cases proc-cases))

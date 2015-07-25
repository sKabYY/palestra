#lang racket

; test
(require "test-cases.rkt")
(require "cps.rkt")
(require "macro.rkt")
(test (lambda (expr)
        (let ((cps (cps-of-expr (macro-expand expr))))
          (pretty-print cps)
          (interp-cps cps)))
      cases)

#lang racket

; test
(require "test-cases.rkt")
(require "cps.rkt")
(require "macro.rkt")
(test (lambda (expr)
        (cps-of-expr (macro-expand expr)))
      cases)

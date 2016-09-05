#!/usr/bin/env racket
#lang racket

(require "parsec.rkt")
(require "test-cases.rkt")

(require "parser.rkt")

;(debug!)
;(exit-at-exception!)

(test (lambda (src) (node->list (parse $program src)))
      cases)

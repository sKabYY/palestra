#lang racket

(require "lib.rkt")
(require "test-cases.rkt")
(require "ceks.rkt")

(interp-disp (lambda (e) (interp e 5)) test-ceks)

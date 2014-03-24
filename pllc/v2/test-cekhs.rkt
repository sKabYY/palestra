#lang racket

(require "lib.rkt")
(require "test-cases.rkt")
(require "cekhs.rkt")

(interp-disp (lambda (e) (interp e 25)) test-cekhs)

#!/usr/bin/env racket
#lang racket

(require "interp.rkt")
(require "test-cases.rkt")

; TODO
(test (lambda (src)
        (let ((out '**uninitialized**))
          (set-output! (lambda (v) (set! out v)))
          (interp src)
          (->imp-value out)))
      cases)

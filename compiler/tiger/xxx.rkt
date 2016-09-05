#!/usr/bin/env racket
#lang racket

(define-syntax mm
  (syntax-rules ()
    [(_ e (pat ...) body ...)
     (match e
       [(list pat ...) body ...]
       [else (error 'error)])]))

(mm '(1 2)
    (x y) (displayln (+ x y)))

#!/usr/bin/env racket
#lang racket

(require "structs.rkt")

(define node
  (new-node 'a (list
                (new-node 'i '(23) #f #f)
                (new-node 'i '(32) #f #f)
                (new-node 's '(1 2 3) #f #f)
                )
            #f #f))

(define (value-of n)
  (nmatch n
    ['a (x y z)
        (+ (value-of x) (value-of y) (value-of z))]
    ['i (i) i]
    ['s l (apply + l)]
    [else 0]))

(displayln (value-of node))

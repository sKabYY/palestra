#lang eopl

(#%require "lib.rkt")

(define (g e0 lst)
  (cons
    e0
    (map (lambda (e) (list (+ (car e) 1) (cadr e))) lst)))

(define (number-elements lst)
  (if (null? lst)
    '()
    (g (list 0 (car lst)) (number-elements (cdr lst)))))

(run-disp
  (number-elements '(a b c d e f g)))

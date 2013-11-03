#lang eopl

(#%provide (all-defined))

(define (displayln o)
  (eopl:pretty-print o) (newline))

(define-syntax run-disp
  (syntax-rules
    ()
    ((_) 'done)
    ((_ a rest ...)
     (begin
       (displayln a)
       (run-disp rest ...)))))

(define (pair a b) (cons a b))
(define (pair-fst p) (car p))
(define (pair-snd p) (cdr p))

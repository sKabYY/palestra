#lang eopl

(#%require "lib.rkt")

; Lc-exp ::= Identiier
;           [var-exp (var)]
;        ::= (lambda ({Identifier}*) Lc-exp)
;           [lambda-exp (bound-vars body)]
;        ::= (Lc-exp {Lc-exp}*)
;           [app-exp (rator rands)]

(define (identifier? v)
  (and (symbol? v)
       (not (eqv? v 'lambda))))

(define (list-of-pred-not-empty predicate v)
  (and (pair? v)
       (predicate (car v))
       (list-of-pred predicate (cdr v))))

(define (list-of-pred predicate v)
  (or
    (null? v)
    (list-of-pred-not-empty predicate v)))

(define (list-of-identifier? v)
  (list-of-pred identifier? v))

(define (list-of-lc-exp? v)
  (list-of-pred lc-exp? v))

(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-vars list-of-identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rands list-of-lc-exp?)))

(define (report-invalid-concrete-syntax datum)
  (eopl:error "Error: invalid syntax" datum))

(define (parse datum)
  (cond
    ((identifier? datum) (var-exp datum))
    ((pair? datum)
     (if (eqv? (car datum) 'lambda)
       (if (= (length datum) 3)
         (lambda-exp
           (cadr datum)
           (parse (caddr datum)))
         (report-invalid-concrete-syntax datum))
       (app-exp
         (parse (car datum))
         (map parse (cdr datum)))))
    (else
      (report-invalid-concrete-syntax datum))))

(define (unparse exp)
  (cases lc-exp exp
    (var-exp (var) var)
    (lambda-exp (bound-vars body)
      (list
        'lambda
        bound-vars
        (unparse body)))
    (app-exp (rator rands)
      (cons (unparse rator)
            (map unparse rands)))))

(define d1 '((lambda (x) (x x)) (lambda (x) (x x))))
(define d2 '(lambda (x y) (x x y)))

(run-disp
  (parse d1)
  (unparse (parse d1))
  (parse d2)
  (unparse (parse d2)))

(displayln (parse '(lambda)))

#lang racket

(require "structs.rkt")

(define (check-program pgm)
  (let ([tenv (init-tenv)])
    (nmatch pgm
      ['program lst
        (let loop ([lst lst])
          (if (null? (cdr lst))
              (void)
              (begin
                (tenv-defun! tenv (car lst))
                (loop (cdr lst)))))
        (let loop ([lst lst])
          (if (null? (cdr lst))
              (check-statement (car lst) (extend-tenv tenv) #t #f)
              (begin
                (check-defun (car lst) (extend-tenv tenv))
                (loop (cdr lst)))))])))

(define (check-statement stm tenv implicit-block? in-fundef?)
  (nmatch stm
    ['block stms]
    ['empty ()]
    ['vardef (var expr)]
    ['if (expr stm1 stm2)]
    ['while (expr stm)]
    ['return ?expr]
    ['output (expr)]
    ['call exprs]
    ['assignment (var expr)]
    ['arrayset (arrref expr)]))

(define (check-expression expr tenv)
  (nmatch expr
    ['arrayref exprs]
    [else (check-simple-expression expr tenv)]))

(define (check-simple-expression expr tenv)
  (nmatch expr
    ['literal (ltl)]
    ['mkarray (type expr)]
    ['andop (expr1 expr2)]
    ['orop (expr1 expr2)]
    ['opd (opd args)]
    ['application exprs]))


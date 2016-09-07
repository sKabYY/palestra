#lang racket

(require "structs.rkt")
(require "error.rkt")

(struct VoidType ())
(struct IntType ())
(struct BoolType ())
(struct ArrayType (elem-type))
(struct FuncType (ret-type arg-types))

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

(define (check-statement stm tenv implicit-block? func-type)
  (define (>> stm tenv)
    (check-statement stm tenv implicit-block? func-type))
  (nmatch stm
    ['block stms (map >> stms)]
    ['empty () (void)]
    ['vardef (var expr)
      (tenv-defvar! tenv
                    (var-id var)
                    (check-expression expr tenv))]
    ['if (expr stm1 stm2)
      (check-bool (check-expression expr tenv))
      (>> stm1 (extend-env tenv))
      (>> stm2 (extend-env tenv))]
    ['while (expr stm)
      (check-bool (check-expression expr))
      (>> stm (extend-env tenv))]
    ['return ?expr
      (cond
        [(not func-type)
         (throw error:return-outside-function "")]
        [(null? ?expr)
         (if )])
      (if func-type
          (check-type-is (FuncType-ret-type func-type)
                         (if (null? ?expr)
                             (VoidType)
                             (check-expression (car ?expr))))
          (throw error:return-outside-function ""))]
    ['output (expr) (check-expression expr tenv)]
    ['call exprs (check-application exprs tenv)]
    ['assignment (var expr)
      (check-type-is (tenv-apply tenv (var-id var))
                     (check-expression expr tenv))]
    ['arrayset (arrref expr)] 'TODO)
  (void))

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

(define (check-application exprs tenv)
  (let ([f-type (check-expression (car exprs) tenv)]
        [a-types (map (lambda (e) (check-expression e tenv))
                      (cdr exprs))])
    'TODO))

(define (var-id var)
  (nmatch var
    ['id (id) id]))

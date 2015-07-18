#lang racket

(provide interp)

(define (interp e)
  (closure-exp1 (value-of (make-closure e (empty-env)))))

; Expression ::= Symbol
;            ::= Number
;            ::= (lambda (Symbol*) Expression)
;            ::= (Expression Expression*)

;            ::= (add1 Expression)
;            ::= (sub1 Expression)
;            ::= (iszero Expression)
;            ::= (+ Expression Expression)
;            ::= (- Expression Expression)
;            ::= (* Expression Expression)

(define (value-of clo)
  (let ((exp1 (closure-exp1 clo))
        (env (closure-env clo)))
    (define (v> e) (value-of (make-closure e env)))
    (define (vv e) (closure-exp1 (v> e)))
    (define (n> n) (make-closure n (empty-env)))
    (match exp1
      ; a variable
      [(? symbol? s) (apply-env env s)]
      ; a basic constant
      [(? number? n) (n> n)]
      ; a procedure
      [`(lambda ,as ,b) (make-closure (make-procedure as b) env)]
      ; an application
      [`(,e1 . ,exps)
       (let ((p (assoc e1 opts)))
         (if p
             (n> (apply (cadr p) (map vv exps)))
             (proc-apply (v> e1) (map v> exps))))])))

(define (add1 x) (+ x 1))
(define (sub1 x) (- x 1))
(define (iszero x) (= x 0))

(define opts
  (list
   (list 'add1 add1)
   (list 'sub1 sub1)
   (list 'iszero iszero)
   (list '+ +)
   (list '- -)
   (list '* *)))

(define (check prad v msg)
  (if (prad v)
      (void)
      (error msg "got:" v)))

; procedure ;;;

(struct procedure (vars body))

(define (make-procedure vars body)
  (check (listof symbol?) vars "need symbols")
  (procedure vars body))

(define (proc-apply clo vals)
  (let ((proc (closure-exp1 clo))
        (env (closure-env clo)))
    (value-of
     (make-closure (procedure-body proc)
                   (extend-env-all env (procedure-vars proc) vals)))))

; environment ;;;

; <val> is a closure
(struct environment (var val closing-env))

(define (empty-env) '())

(define (extend-env env var val)
  (environment var val env))

(define (extend-env-all env vars vals)
  ; assert (= (length vars) (length vals))
  (if (null? vars)
      env
      (extend-env-all (extend-env env (car vars) (car vals))
                      (cdr vars)
                      (cdr vals))))

(define (apply-env env search-var)
  (if (null? env)
      (make-closure search-var (empty-env))
      (let ((var (environment-var env))
            (val (environment-val env))
            (closing-env (environment-closing-env env)))
        (if (eqv? search-var var)
            val
            (apply-env closing-env search-var)))))

; closure ;;;

(struct closure (exp1 env))

(define (make-closure exp1 env)
  (closure exp1 env))

;(require "lib.rkt")
;(driver-loop interp)

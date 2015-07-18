#lang racket

(provide interp)

(define (interp e)
  (closure-exp1 (value-of/k e (empty-env) (end-cont))))

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

(define (value-of/k exp1 env cont)
  (match exp1
    ; a variable
    [(? symbol? s) (apply-cont cont (apply-env env s))]
    ; a basic constant
    [(? number? n) (apply-cont cont (make-closure n (empty-env)))]
    ; a procedure
    [`(lambda ,as ,b)
     (apply-cont cont (make-closure (make-procedure as b) env))]
    ; an application
    [`(,e1 . ,exps)
     (let ((p (assoc e1 opts)))
       (if p
           (if (null? exps)
               (apply-cont cont (opt-apply (cadr p) '()))
               (value-of/k (car exps)
                           env
                           (opt-cont cont (cadr p) (cdr exps) env '())))
           (value-of/k e1 env (args-cont cont exps env))))]))

; continuation ;;;

(define (end-cont)
  (lambda (v)
    (displayln "###Done!###")
    v))

(define (opt-cont cont opt exps env vals)
  (lambda (v)
    (let ((new-vals (cons (closure-exp1 v) vals)))
      (if (null? exps)
          (apply-cont cont (make-closure (apply opt (reverse new-vals))
                                         (empty-env)))
          (value-of/k (car exps)
                      env
                      (opt-cont cont opt (cdr exps) env new-vals))))))

(define (args-cont cont exps env)
  (lambda (v)
    (if (null? exps)
        (proc-apply/k v '() cont)
        (value-of/k (car exps) env (fun-cont cont v (cdr exps) env '())))))

(define (fun-cont cont rator exps env vals)
  (lambda (v)
    (let ((new-vals (cons v vals)))
      (if (null? exps)
          (proc-apply/k rator (reverse new-vals) cont)
          (value-of/k (car exps)
                      env
                      (fun-cont cont rator (cdr exps) env new-vals))))))

(define (apply-cont cont v) (cont v))

(define (opt-apply opt vals)
  (make-closure (apply opt vals) (empty-env)))

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

(define (proc-apply/k clo vals cont)
  (let ((proc (closure-exp1 clo))
        (env (closure-env clo)))
    (value-of/k (procedure-body proc)
                (extend-env-all env (procedure-vars proc) vals)
                cont)))

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

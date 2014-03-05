#lang racket

(provide interp)

(define (interp e)
  (closure-exp1 (value-of (make-closure e (empty-env)))))

; Expression ::= Symbol
;            ::= Number
;            ::= (lambda (Symbol) Expression)
;            ::= (add1 Expression)
;            ::= (sub1 Expression)
;            ::= (+ Expression Expression)
;            ::= (- Expression Expression)
;            ::= (* Expression Expression)
;            ::= (Expression Expression)

(define (value-of clo)
  (let ((exp1 (closure-exp1 clo))
        (env (closure-env clo)))
    (define (v> e) (value-of (make-closure e env)))
    (define (vv e) (closure-exp1 (v> e)))
    (make-closure
     (match exp1
       ; a variable
       [(? symbol? s) (apply-env env s)]
       ; a basic constant
       [(? number? x) x]
       ; a procedure
       [`(lambda (,a) ,b) (make-procedure a b)]
       ; primitive operations
       [`(add1 ,e) (+ (vv e) 1)]
       [`(sub1 ,e) (- (vv e) 1)]
       [`(iszero ,e) (= (vv e) 0)]
       [`(+ ,e1 ,e2) (+ (vv e1) (vv e2))]
       [`(- ,e1 ,e2) (- (vv e1) (vv e2))]
       [`(* ,e1 ,e2) (* (vv e1) (vv e2))]
       ; an application
       [`(,e1 ,e2) (proc-apply (v> e1) (vv e2))])
     env)))

(define (check prad v msg)
  (if (prad v)
      (void)
      (error msg "got:" v)))

; procedure ;;;

(struct procedure (var body))

(define (make-procedure var body)
  (check symbol? var "need symbol")
  (procedure var body))

(define (proc-apply clo val)
  (let ((proc (closure-exp1 clo))
        (env (closure-env clo)))
    (closure-exp1
     (value-of
      (make-closure (procedure-body proc)
                    (extend-env env (procedure-var proc) val))))))

; environment ;;;

(struct environment (var val closing-env))

(define (empty-env) '())

(define (extend-env env var val)
  (environment var val env))

(define (apply-env env search-var)
  (if (null? env)
      search-var
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

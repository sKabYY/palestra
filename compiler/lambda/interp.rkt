#lang racket
(provide interp)

(require "macro.rkt")

(define (interp expr)
  (value-of/k (macro-expand expr)
              (empty-env)
              (end-cont)))

(define (value-of/k expr env cont)
  (match expr
    ; a variable
    [(? symbol? s) (apply-cont cont (apply-env env s))]
    ; a number
    [(? number? n) (apply-cont cont n)]
    ; a boolean
    [(? boolean? b) (apply-cont cont b)]
    ; a procedure
    [`(func (,a) ,body) (apply-cont cont (closure a body env))]
    ; an if expression
    [`(if ,e1 ,e2, e3) (value-of/k e1 env (if-cont cont env e2 e3))]
    ; a diff operation
    [`(- ,e1 ,e2) (value-of/k e1 env (diff-cont cont env e2))]
    ; an iszero operation
    [`(zero? ,e1) (value-of/k e1 env (zero?-cont cont))]
    ; an application
    [`(,e1 ,e2) (value-of/k e1 env (arg-cont cont env e2))]))

; closure
(define (closure a body env)
  (lambda (v cont)
    (value-of/k body (extend-env env a v) cont)))

(define (apply-closure/k clo v cont) (clo v cont))

; environment
(define (empty-env)
  (lambda (s) (report-unbound-var s)))

(define (extend-env env a v)
  (lambda (s)
    (if (eqv? a s)
        v
        (apply-env env s))))

(define (apply-env env s) (env s))

; continuation
(define (end-cont)
  (lambda (v) (print-info ">> Done!") v))

(define (if-cont cont env e2 e3)
  (lambda (v)
    (if v
        (value-of/k e2 env cont)
        (value-of/k e3 env cont))))

(define (diff-cont cont env e2)
  (lambda (v1)
    (value-of/k e2 env (diff2-cont cont v1))))

(define (diff2-cont cont v1)
  (lambda (v2)
    (apply-cont cont (- v1 v2))))

(define (zero?-cont cont)
  (lambda (v)
    (if (= v 0)
        (apply-cont cont #t)
        (apply-cont cont #f))))

(define (arg-cont cont env e2)
  (lambda (v)
    (value-of/k e2 env (func-cont cont v))))

(define (func-cont cont clo)
  (lambda (v)
    (apply-closure/k clo v cont)))

(define (apply-cont cont v) (cont v))

; utils
(define (print-info msg) (displayln msg))

; report error
(define (report-unbound-var s)
  (error "[Error] unbound var:" s))

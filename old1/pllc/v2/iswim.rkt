#lang racket

(provide interp)

(define (interp e) (value-of e))

; Expression ::= Symbol
;            ::= Number
;            ::= (lambda (Symbol) Expression)
;            ::= (add1 Expression)
;            ::= (sub1 Expression)
;            ::= (+ Expression Expression)
;            ::= (- Expression Expression)
;            ::= (* Expression Expression)
;            ::= (Expression Expression)

(define (value-of exp1)
  (match exp1
    ; a variable
    [(? symbol? s) s]
    ; a basic constant
    [(? number? x) x]
    ; a procedure
    [`(lambda (,a) ,b) (make-procedure a b)]
    ; primitive operations
    [`(add1 ,e) (+ (value-of e) 1)]
    [`(sub1 ,e) (- (value-of e) 1)]
    [`(iszero ,e) (= (value-of e) 0)]
    [`(+ ,e1 ,e2) (+ (value-of e1) (value-of e2))]
    [`(- ,e1 ,e2) (- (value-of e1) (value-of e2))]
    [`(* ,e1 ,e2) (* (value-of e1) (value-of e2))]
    ; an application
    [`(,e1 ,e2) (proc-apply (value-of e1) (value-of e2))]))

(define (substitute var val exp1)
  (define (>> e) (substitute var val e))
  (match exp1
    ; a variable
    [(? symbol? s)
     (if (eqv? s var) val s)]
    ; a basic constant
    [(? number? x) x]
    ; a procedure
    [`(lambda (,a) ,b)
     (if (eqv? a var)
         exp1
         `(lambda (,a) ,(>> b)))]
    ; primitive operations
    [`(add1 ,e) `(add1 ,(>> e))]
    [`(sub1 ,e) `(sub1 ,(>> e))]
    [`(iszero ,e) `(iszero ,(>> e))]
    [`(+ ,e1 ,e2) `(+ ,(>> e1) ,(>> e2))]
    [`(- ,e1 ,e2) `(- ,(>> e1) ,(>> e2))]
    [`(* ,e1 ,e2) `(+ ,(>> e1) ,(>> e2))]
    ; an application
    [`(,e1 ,e2) `(,(>> e1) ,(>> e2))]))

(define (check prad v msg)
  (if (prad v)
      (void)
      (error msg "got:" v)))

; procedure ;;;

(struct procedure (var body))

(define (make-procedure var body)
  (check symbol? var "need symbol")
  (procedure var body))

(define (proc-apply proc val)
  (value-of (substitute (procedure-var proc) val (procedure-body proc))))

;(require "lib.rkt")
;(driver-loop interp)

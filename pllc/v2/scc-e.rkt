#lang racket

(provide interp)

(define (interp e) (value-of e (empty-env)))

; Expression = Symbol
;            | Number
;            | (lambda (Symbol*) Expression)
;            | (Expression Expression*)
;            | (add1 Expression)
;            | (sub1 Expression)
;            | (iszero Expression)
;            | (+ Expression Expression)
;            | (- Expression Expression)
;            | (* Expression Expression)

; Value      = Symbol
;            | Number
;            | Closure

(define (value-of exp1 env)
  (define (v> e) (value-of e env))
  (match exp1
    ; a variable
    [(? symbol? s) (apply-env env s)]
    ; a basic constant
    [(? number? n) n]
    ; a procedure
    [`(lambda ,as ,b) (make-closure as b env)]
    ; an application
    [`(,e1 . ,exps)
     (let* ((rator (v> e1))
            (p (find-opt rator))
            (vals (map v> exps)))
       (if p
           (opt-apply p vals)
           (proc-apply rator vals)))]))

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

(define (find-opt s) (assoc s opts))

(define (opt-apply p vals) (apply (cadr p) vals))

(define (check prad v msg)
  (if (prad v)
      (void)
      (error msg "got:" v)))

; closure ;;;

(struct closure (vars body env))

(define (make-closure vars body env)
  (closure vars body env))

(define (proc-apply clo vals)
  (let ((vars (closure-vars clo))
        (body (closure-body clo))
        (env (closure-env clo)))
    (value-of body (extend-env-all env vars vals))))

; environment ;;;

; <val> is a value
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
      search-var
      (let ((var (environment-var env))
            (val (environment-val env))
            (closing-env (environment-closing-env env)))
        (if (eqv? search-var var)
            val
            (apply-env closing-env search-var)))))

;(require "lib.rkt")
;(driver-loop interp)

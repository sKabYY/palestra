#lang racket

(provide interp)

(define (interp e) (value-of e))

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

(define the-exp 'uninitialized)
(define the-env 'uninitialized)
(define the-cont 'uninitialized)
(define the-pc 'uninitialized)

(define (set-exp! v) (set! the-exp v))
(define (set-env! v) (set! the-env v))
(define (set-cont! v) (set! the-cont v))
(define (set-pc! v) (set! the-pc v))

(define (print-all)
  (define (print-one v)
    (pretty-print v))
  (displayln "---")
  (print-one the-exp)
  (print-one the-env)
  (print-one the-cont)
  (print-one the-pc)
  (displayln "==="))

(define (value-of e)
  (set-exp! e)
  (set-env! (empty-env))
  (set-cont! (end-cont))
  (set-pc! value-of/k)
  (trampoline!))

(define (trampoline!)
  (print-all)
  (if the-pc
      (begin
        (the-pc)
        (trampoline!))
      the-exp))

(define (value-of/k)
  (define (goto-apply-cont)
    (set-env! (empty-env))
    (set-pc! apply-cont))
  (match the-exp
    ; a variable
    [(? symbol? s)
     (set-exp! (apply-env the-env s))
     (goto-apply-cont)]
    ; a basic constant
    [(? number? n)
     (set-exp! n)
     (goto-apply-cont)]
    ; a procedure
    [`(lambda ,as ,b)
     (set-exp! (make-closure as b the-env))
     (goto-apply-cont)]
    ; an application
    [`(,e1 . ,exps)
     (let ((p (assoc e1 opts)))
       (if p
           (if (null? exps)
               (begin
                 (set-exp! (opt-apply p '()))
                 (goto-apply-cont))
               (begin
                 (set-exp! (car exps))
                 (set-cont! (opt-cont the-cont p (cdr exps) the-env '()))))
           (begin
             (set-exp! e1)
             (set-cont! (arg-cont the-cont exps the-env)))))]))

; continuation ;;;

;
(struct end-cont () #:transparent)
(struct opt-cont (cont opt exps env vals) #:transparent)
(struct arg-cont (cont exps env) #:transparent)
(struct fun-cont (cont fun exps env vals) #:transparent)
;

(define (apply-cont)
  (cond
   [(end-cont? the-cont) (set-pc! #f)]
   [(opt-cont? the-cont)
    (let ((cont (opt-cont-cont the-cont))
          (opt (opt-cont-opt the-cont))
          (exps (opt-cont-exps the-cont))
          (env (opt-cont-env the-cont))
          (vals (opt-cont-vals the-cont)))
      (let ((new-vals (cons the-exp vals)))
        (if (null? exps)
            (begin
              (set-cont! cont)
              (set-exp! (opt-apply opt (reverse new-vals)))
              (set-env! (empty-env)))
            (begin
              (set-cont! (opt-cont cont opt (cdr exps) env new-vals))
              (set-exp! (car exps))
              (set-env! env)
              (set-pc! value-of/k)))))]
   [(arg-cont? the-cont)
    (let ((cont (arg-cont-cont the-cont))
          (exps (arg-cont-exps the-cont))
          (env (arg-cont-env the-cont)))
      (if (null? exps)
          (proc-apply/k the-exp '() cont)
          (begin
            (set-cont! (fun-cont cont the-exp (cdr exps) env '()))
            (set-exp! (car exps))
            (set-env! env)
            (set-pc! value-of/k))))]
   [(fun-cont? the-cont)
    (let ((cont (fun-cont-cont the-cont))
          (fun (fun-cont-fun the-cont))
          (exps (fun-cont-exps the-cont))
          (env (fun-cont-env the-cont))
          (vals (fun-cont-vals the-cont)))
      (let ((new-vals (cons the-exp vals)))
        (if (null? exps)
            (proc-apply/k fun (reverse new-vals) cont)
            (begin
              (set-cont! (fun-cont cont fun (cdr exps) env new-vals))
              (set-exp! (car exps))
              (set-env! env)
              (set-pc! value-of/k)))))]))


(define (opt-apply opt vals) (apply (cadr opt) vals))

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

; closure ;;;

(struct closure (vars body env) #:transparent)

(define (make-closure vars body env)
  (closure vars body env))

(define (proc-apply/k clo vals cont)
  (let ((vars (closure-vars clo))
        (body (closure-body clo))
        (env (closure-env clo)))
    (set-exp! body)
    (set-env! (extend-env-all env vars vals))
    (set-cont! cont)
    (set-pc! value-of/k)))

; environment ;;;

; <val> is a closure
(struct environment (var val closing-env) #:transparent)
(struct empty-env () #:transparent)

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
  (if (empty-env? env)
      search-var
      (let ((var (environment-var env))
            (val (environment-val env))
            (closing-env (environment-closing-env env)))
        (if (eqv? search-var var)
            val
            (apply-env closing-env search-var)))))

;(require "lib.rkt")
;(driver-loop interp)

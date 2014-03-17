#lang racket

(provide init-store interp)

(define (interp e store-size)
  (init-store store-size)
  (value-of/k e (empty-env) (end-cont)))

; Expression = Symbol
;            | Number
;            | (lambda (Symbol*) Expression)
;            | (Expression Expression*)
;            | (set! Symbol Expression)

;            | (add1 Expression)
;            | (sub1 Expression)
;            | (iszero Expression)
;            | (+ Expression Expression)
;            | (- Expression Expression)
;            | (* Expression Expression)

(define the-store 'uninitialized)
(define the-store-size 'uninitialized)

(define (init-store size)
  (set! the-store-size 0)
  (set! the-store (make-vector size)))

(define (newref val)
  (let ((ref the-store-size))
    (set! the-store-size (+ ref 1))
    (vector-set! the-store ref val)
    ref))

(define (deref ref) (vector-ref the-store ref))

(define (setref! ref val) (vector-set! the-store ref val))

(define (value-of/k exp1 env cont)
  (match exp1
    ; a variable
    [(? symbol? s)
     (apply-cont cont (apply-env env s))]
    ; a basic constant
    [(? number? n) (apply-cont cont n)]
    ; a procedure
    [`(lambda ,as ,b)
     (apply-cont cont (make-closure as b env))]
    ; an assignment
    [`(set! ,s ,e1)
     (value-of/k e1 env (set-cont cont env s))]
    ; a begin statement
    [`(begin ,e1 . ,exps)
     (value-of/k e1 env (begin-cont cont env exps))]
    ; a let statement
    [`(let ,defs ,e1)
     (value-of/k (translate-let defs e1) env cont)]
    ; an application
    [`(,e1 . ,exps)
     (let ((p (assoc e1 opts)))
       (if p
           (if (null? exps)
               (apply-cont cont (opt-apply p '()))
               (value-of/k (car exps)
                           env
                           (opt-cont cont p (cdr exps) env '())))
           (value-of/k e1 env (args-cont cont exps env))))]))

(define (translate-let defs e1)
  (define (iter vars exps rest e1)
    (if (null? rest)
        `((lambda ,vars ,e1) . ,exps)
        (match (car rest)
          [(list (? symbol? var) e)
           (iter (cons var vars) (cons e exps) (cdr rest) e1)])))
  (iter '() '() defs e1))

; continuation ;;;

;
(define (end-cont)
  (lambda (v)
    (displayln the-store)
    (displayln "###Done!###")
    v))

(define (opt-cont cont opt exps env vals)
  (lambda (v)
    (let ((new-vals (cons v vals)))
      (if (null? exps)
          (apply-cont cont (opt-apply opt (reverse new-vals)))
          (value-of/k (car exps)
                      env
                      (opt-cont cont opt (cdr exps) env new-vals))))))

(define (args-cont cont exps env)
  (lambda (v)
    (if (null? exps)
        (proc-apply/k v '() cont)
        (value-of/k (car exps) env (fun-cont cont v (cdr exps) env '())))))

(define (fun-cont cont fun exps env vals)
  (lambda (v)
    (let ((new-vals (cons v vals)))
      (if (null? exps)
          (proc-apply/k fun (reverse new-vals) cont)
          (value-of/k (car exps)
                      env
                      (fun-cont cont fun (cdr exps) env new-vals))))))

(define (set-cont cont env s)
  (lambda (v)
    (setref! (apply-env-ref env s) v)
    (apply-cont cont (void))))

(define (begin-cont cont env exps)
  (lambda (v)
    (if (null? exps)
        (apply-cont cont v)
        (value-of/k (car exps)
                    env
                    (begin-cont cont env (cdr exps))))))
;

(define (apply-cont cont v) (cont v))

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

(struct closure (vars body env))

(define (make-closure vars body env)
  (closure vars body env))

(define (proc-apply/k clo vals cont)
  (let ((vars (closure-vars clo))
        (body (closure-body clo))
        (env (closure-env clo)))
    (value-of/k body (extend-env-all env vars vals) cont)))

; environment ;;;

(struct environment (var ref closing-env) #:transparent)

(define (empty-env) '())

(define (extend-env env var val)
  (environment var (newref val) env))

(define (extend-env-all env vars vals)
  ; assert (= (length vars) (length vals))
  (if (null? vars)
      env
      (extend-env-all (extend-env env (car vars) (car vals))
                      (cdr vars)
                      (cdr vals))))

(define (apply-env env search-var)
  (deref (apply-env-ref env search-var)))

(define (apply-env-ref env search-var)
  (if (null? env)
      (report-unbound-var-error search-var)
      (let ((var (environment-var env))
            (ref (environment-ref env))
            (closing-env (environment-closing-env env)))
        (if (eqv? search-var var)
            (begin
              ref)
            (apply-env-ref closing-env search-var)))))

(define (report-unbound-var-error var)
  (error "unbound var:" var))

;(require "lib.rkt")
;(driver-loop interp)

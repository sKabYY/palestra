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

; registers
(define pc 'uninitialized)
(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
;(define proc 'uninitialized)
;(define args 'uninitialized)
(define val 'uninitialized)

(define (value-of exp1)
  (set! pc value-of/k)
  (set! exp exp1)
  (set! env (empty-env))
  (set! cont (end-cont))
  (trampoline!)
  val)

(define (trampoline!)
  (printf "###~n")
  (printf "  ~a~n" pc)
  (printf "  ~a~n" exp)
  (printf "  ~a~n" env)
  (printf "  ~a~n" cont)
  (printf "  ~a~n" val)
  (printf "###~n")
  (if pc
      (begin
       (pc)
       (trampoline!))
      (void)))

(define (value-of/k)
  (match exp
    ; a variable
    [(? symbol? s)
     (set! val (apply-env env s))
     (set! pc apply-cont)]
    ; a basic constant
    [(? number? n)
     (set! val n)
     (set! pc apply-cont)]
    ; a procedure
    [`(lambda ,as ,b)
     (set! val (make-closure as b env))
     (set! pc apply-cont)]
    ; an application
    [`(,e1 . ,exps)
     (let ((p (assoc e1 opts)))
       (if p
           (if (null? exps)
               (begin
                 (set! val (opt-apply p '()))
                 (set! pc apply-cont))
               (begin
                 (set! exp (car exps))
                 (set! cont (opt-cont cont p (cdr exps) env '()))
                 (set! pc value-of/k)))
           (begin
             (set! exp e1)
             (set! cont (args-cont cont exps env))
             (set! pc value-of/k))))]))

; continuation ;;;

;
(define (end-cont)
  (lambda ()
    (displayln "###Done!###")
    (set! pc #f)))

(define (opt-cont cont1 opt exps env1 vals)
  (lambda ()
    (let ((new-vals (cons val vals)))
      (set! env env1)
      (if (null? exps)
          (begin
            (set! cont cont1)
            (set! val (opt-apply opt (reverse new-vals)))
            (set! pc apply-cont))
          (begin
            (set! exp (car exps))
            (set! cont (opt-cont cont1 opt (cdr exps) env1 new-vals))
            (set! pc value-of/k))))))

(define (args-cont cont1 exps env1)
  (lambda ()
    (if (null? exps)
        (proc-apply/k val '() cont1)
        (begin
          (set! exp (car exps))
          (set! env env1)
          (set! cont (fun-cont cont1 val (cdr exps) env '()))
          (set! pc value-of/k)))))

(define (fun-cont cont1 rator exps env1 vals)
  (lambda ()
    (let ((new-vals (cons val vals)))
      (if (null? exps)
          (proc-apply/k rator (reverse new-vals) cont1)
          (begin
            (set! exp (car exps))
            (set! env env1)
            (set! cont (fun-cont cont1 rator (cdr exps) env new-vals))
            (set! pc value-of/k))))))
;

(define (apply-cont) (cont))

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

(define (proc-apply/k clo vals cont1)
  (let ((vars (closure-vars clo))
        (body (closure-body clo))
        (env1 (closure-env clo)))
    (set! exp body)
    (set! env (extend-env-all env1 vars vals))
    (set! cont cont1)
    (set! pc value-of/k)))

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
      search-var
      (let ((var (environment-var env))
            (val (environment-val env))
            (closing-env (environment-closing-env env)))
        (if (eqv? search-var var)
            val
            (apply-env closing-env search-var)))))

;(require "lib.rkt")
;(driver-loop interp)

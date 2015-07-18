#lang racket

(provide init-store interp)

(define (interp e store-size)
  (init-store store-size)
  ; (pretty-print (translation-of e))
  (eval/k (translation-of e) (empty-env) (end-cont)))

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
(define the-free-list 'uninitialized)

(define (init-store size)
  (set! the-store-size 0)
  (set! the-free-list '())
  (set! the-store (make-vector size)))

(define (newref val)
  (define (alloc-one)
    (if (null? the-free-list)
        (let ((ref the-store-size))
          (set! the-store-size (+ ref 1))
          ref)
        (let ((ref (car the-free-list)))
          (set! the-free-list (cdr the-free-list))
          ref)))
  (let ((ref (alloc-one)))
    (vector-set! the-store ref val)
    ref))

(define (deref ref) (vector-ref the-store ref))

(define (setref! ref val) (vector-set! the-store ref val))

(define (translation-of exp1)
  (match exp1
    ; a procedure
    [`(lambda ,as ,b)
     `(lambda ,as ,(translation-of b))]
    ; a let statement
    [`(let ,defs ,e1)
     (translate-let defs e1)]
    [`(set! ,s ,e1)
     `(set! ,s ,(translation-of e1))]
    [`(begin ,exps)
     `(begin ,(map translation-of exps))]
    [`(,e1 . ,exps)
     (cons (translation-of e1)
           (map translation-of exps))]
    [else
     exp1]))

(define (translate-let defs e1)
  (define (iter vars exps rest e1)
    (if (null? rest)
        `((lambda ,vars ,(translation-of e1)) . ,(map translation-of exps))
        (match (car rest)
          [(list (? symbol? var) e)
           (iter (cons var vars) (cons e exps) (cdr rest) e1)])))
  (iter '() '() defs e1))

(define (eval/k exp1 env cont)
  (define (apply-cont cont v)
    (cond
     [(end-cont? cont) (apply-end-cont cont v)]
     [(opt-cont? cont) (apply-opt-cont cont v)]
     [(arg-cont? cont) (apply-arg-cont cont v)]
     [(fun-cont? cont) (apply-fun-cont cont v)]
     [(set-cont? cont) (apply-set-cont cont v)]
     [(begin-cont? cont) (apply-begin-cont cont v)]))
  (define (value? v)
    (or (void? v)
        (boolean? v)
        (number? v)
        (closure? v)))
  ; gc
  (gc exp1 env cont)
  ; eval
  (match exp1
    ; a value
    [(? value? v)
     (apply-cont cont v)]
    ; a variable
    [(? symbol? s)
     (apply-cont cont (apply-env env s))]
    ; a procedure
    [`(lambda ,as ,b)
     (eval/k (make-closure as b env) (empty-env) cont)]
    ; an assignment
    [`(set! ,s ,e1)
     (eval/k e1 env (set-cont cont env s))]
    ; a begin statement
    [`(begin ,e1 . ,exps)
     (eval/k e1 env (begin-cont cont env exps))]
    ; an application
    [`(,e1 . ,exps)
     (let ((p (assoc e1 opts)))
       (if p
           (if (null? exps)
               (apply-cont cont (opt-apply p '()))
               (eval/k (car exps)
                       env
                       (opt-cont cont env p (cdr exps) '())))
           (eval/k e1 env (arg-cont cont env exps))))]))

; continuation ;;;

;
(struct end-cont ())
(define (apply-end-cont cont1 v)
  (printf "store: ~a~n" the-store)
  (displayln "###Done!###")
  v)

(struct opt-cont (cont env opt exps vals))
(define (apply-opt-cont cont1 v)
  (let ((cont (opt-cont-cont cont1))
        (env (opt-cont-env cont1))
        (opt (opt-cont-opt cont1))
        (exps (opt-cont-exps cont1))
        (vals (opt-cont-vals cont1)))
    (let ((new-vals (cons v vals)))
      (if (null? exps)
          (eval/k (opt-apply opt (reverse new-vals)) (empty-env) cont)
          (eval/k (car exps)
                  env
                  (opt-cont cont env opt (cdr exps) new-vals))))))

(struct arg-cont (cont env exps))
(define (apply-arg-cont cont1 v)
  (let ((cont (arg-cont-cont cont1))
        (env (arg-cont-env cont1))
        (exps (arg-cont-exps cont1)))
    (if (null? exps)
        (proc-apply/k v '() cont)
        (eval/k (car exps) env (fun-cont cont env v (cdr exps) '())))))

(struct fun-cont (cont env fun exps vals))
(define (apply-fun-cont cont1 v)
  (let ((cont (fun-cont-cont cont1))
        (env (fun-cont-env cont1))
        (fun (fun-cont-fun cont1))
        (exps (fun-cont-exps cont1))
        (vals (fun-cont-vals cont1)))
    (let ((new-vals (cons v vals)))
      (if (null? exps)
          (proc-apply/k fun (reverse new-vals) cont)
          (eval/k (car exps)
                  env
                  (fun-cont cont env fun (cdr exps) new-vals))))))

(struct set-cont (cont env s))
(define (apply-set-cont cont1 v)
  (let ((cont (set-cont-cont cont1))
        (env (set-cont-env cont1))
        (s (set-cont-s cont1)))
    (setref! (apply-env-ref env s) v)
    (eval/k (void) (empty-env) cont)))

(struct begin-cont (cont env exps))
(define (apply-begin-cont cont1 v)
  (let ((cont (begin-cont-cont cont1))
        (env (begin-cont-env cont1))
        (exps (begin-cont-exps cont1)))
    (if (null? exps)
        (eval/k v (empty-env) cont)
        (eval/k (car exps)
                env
                (begin-cont cont env (cdr exps))))))

(define (cont-env cont)
  (cond
   [(opt-cont? cont) (opt-cont-env cont)]
   [(arg-cont? cont) (arg-cont-env cont)]
   [(fun-cont? cont) (fun-cont-env cont)]
   [(set-cont? cont) (set-cont-env cont)]
   [(begin-cont? cont) (begin-cont-env cont)]))

(define (cont-cont cont)
  (cond
   [(opt-cont? cont) (opt-cont-cont cont)]
   [(arg-cont? cont) (arg-cont-cont cont)]
   [(fun-cont? cont) (fun-cont-cont cont)]
   [(set-cont? cont) (set-cont-cont cont)]
   [(begin-cont? cont) (begin-cont-cont cont)]))
;

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
    (eval/k body (extend-env-all env vars vals) cont)))

; environment ;;;

(struct environment (var ref closing-env) #:transparent)

(struct empty-env ())

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
  (if (empty-env? env)
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

; gc ;;

(define (gc exp1 env cont)
  (define flags (make-vector the-store-size 'white))
  (define grays '())
  (define (mark-env env)
    (if (empty-env? env)
        (void)
        (let ((ref (environment-ref env))
              (closing-env (environment-closing-env env)))
          (if (eqv? (vector-ref flags ref) 'white)
              (begin
                (vector-set! flags ref 'gray)
                (set! grays (cons ref grays))
                (mark-env closing-env))
              (void)))))
  (define (mark-cont cont)
    (if (end-cont? cont)
        (void)
        (begin
          (mark-env (cont-env cont))
          (mark-cont (cont-cont cont)))))
  (define (set-free-list!)
    (define (whites ref)
      (if (< ref 0)
          '()
          (if (eqv? (vector-ref flags ref) 'white)
              (cons ref (whites (- ref 1)))
              (whites (- ref 1)))))
    (set! the-free-list (whites (- the-store-size 1))))
  (define (iter)
    (if (null? grays)
        (void)
        (begin
          (let* ((ref (car grays))
                 (v (deref ref)))
            (set! grays (cdr grays))
            (vector-set! flags ref 'black)
            (if (closure? v)
                (mark-env (closure-env v))
                (void)))
          (iter))))
  (begin
    (mark-env env)
    (mark-cont cont)
    (iter)
    (set-free-list!)))

;

;(require "lib.rkt")
;(driver-loop interp)

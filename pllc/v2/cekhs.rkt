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
;            | (letcc Symbol Expression)
;            | (cc Expression Expression)
;            | (throw Expression)
;            | (catch Expression with Symbol Expression)

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
     [(continuation? cont) (apply-continuation cont v)]))
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
    ; a letcc statement
    [`(letcc ,s ,e1)
     (eval/k e1 (extend-env env s cont) cont)]
    ; a cc statement
    [`(cc ,e1 ,e2)
     (eval/k e1 env (ccval-cont cont env e2))]
    ; a throw statement
    [`(throw ,e1)
     (eval/k e1 env (throw-cont cont))]
    ; a catch statement
    [`(catch ,e1 with ,s ,e2)
     (eval/k e1 env (catch-cont cont env s e2))]
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

(define (end-program msg v)
  (printf "store: ~a~n" the-store)
  (displayln msg)
  v)

(struct end-cont ())
(define (apply-end-cont cont1 v)
  (end-program "###Done!###" v))

(struct continuation (cont env data))

(struct opt-data (opt exps vals))
(define (opt-cont cont env opt exps vals)
  (continuation cont env (opt-data opt exps vals)))

(struct arg-data (exps))
(define (arg-cont cont env exps)
  (continuation cont env (arg-data exps)))

(struct fun-data (fun exps vals))
(define (fun-cont cont env fun exps vals)
  (continuation cont env (fun-data fun exps vals)))

(struct set-data (s))
(define (set-cont cont env s)
  (continuation cont env (set-data s)))

(struct begin-data (exps))
(define (begin-cont cont env exps)
  (continuation cont env (begin-data exps)))

(struct ccval-data (e))
(define (ccval-cont cont env e)
  (continuation cont env (ccval-data e)))

(struct throw-data ())
(define (throw-cont cont)
  (continuation cont (empty-env) (throw-data)))

(struct catch-data (s e))
(define (catch-cont cont env s e)
  (continuation cont env (catch-data s e)))

(define (apply-continuation cont1 v)
  (let ((cont (continuation-cont cont1))
        (env (continuation-env cont1))
        (data (continuation-data cont1)))
    (cond
     [(opt-data? data)
      (let ((opt (opt-data-opt data))
            (exps (opt-data-exps data))
            (vals (opt-data-vals data)))
        (apply-opt-cont cont env opt exps vals v))]
     [(arg-data? data)
      (let ((exps (arg-data-exps data)))
        (apply-arg-cont cont env exps v))]
     [(fun-data? data)
      (let ((fun (fun-data-fun data))
            (exps (fun-data-exps data))
            (vals (fun-data-vals data)))
        (apply-fun-cont cont env fun exps vals v))]
     [(set-data? data)
      (let ((s (set-data-s data)))
        (apply-set-cont cont env s v))]
     [(begin-data? data)
      (let ((exps (begin-data-exps data)))
        (apply-begin-cont cont env exps v))]
     [(ccval-data? data)
      (let ((e (ccval-data-e data)))
        (apply-ccval-cont cont env e v))]
     [(throw-data? data)
      (apply-throw-cont cont v)]
     [(catch-data? data)
      (eval/k v (empty-env) cont)])))

(define (apply-opt-cont cont env opt exps vals v)
  (let ((new-vals (cons v vals)))
    (if (null? exps)
        (eval/k (opt-apply opt (reverse new-vals)) (empty-env) cont)
        (eval/k (car exps)
                env
                (opt-cont cont env opt (cdr exps) new-vals)))))

(define (apply-arg-cont cont env exps v)
  (if (null? exps)
      (proc-apply/k v '() cont)
      (eval/k (car exps) env (fun-cont cont env v (cdr exps) '()))))

(define (apply-fun-cont cont env fun exps vals v)
  (let ((new-vals (cons v vals)))
    (if (null? exps)
        (proc-apply/k fun (reverse new-vals) cont)
        (eval/k (car exps)
                env
                (fun-cont cont env fun (cdr exps) new-vals)))))

(define (apply-set-cont cont env s v)
  (setref! (apply-env-ref env s) v)
  (eval/k (void) (empty-env) cont))

(define (apply-begin-cont cont env exps v)
  (if (null? exps)
      (eval/k v (empty-env) cont)
      (eval/k (car exps)
              env
              (begin-cont cont env (cdr exps)))))

(define (apply-ccval-cont cont env e v)
  (eval/k e env v))

(define (apply-throw-cont cont v)
  (define (catch-cont? cont)
    (catch-data? (continuation-data cont)))
  (if (end-cont? cont)
      (end-program "###Error!###" v)
      (let ((data (continuation-data cont)))
        (if (catch-data? data)
            (let ((s (catch-data-s data))
                  (e (catch-data-e data)))
              (eval/k e (extend-env (continuation-env cont) s v) cont))
            (apply-throw-cont (continuation-cont cont) v)))))
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
          (mark-env (continuation-env cont))
          (mark-cont (continuation-cont cont)))))
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

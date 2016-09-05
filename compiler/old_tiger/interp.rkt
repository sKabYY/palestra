#lang racket

(provide (combine-out interp
                      set-output!
                      ->imp-value))

(define (interp pgm) (interp-pgm pgm))

(define (->imp-value val)
  (let ((t (exptype val))
        (v (expval val)))
    (if (type-array? t)
        (vector-map ->imp-value v)
        v)))
; TODO: hook print

(define (set-output! f) (set! output f))
(define output displayln)

(define (interp-pgm pgm)
  (match pgm
    [`((main . ,stms) . ,fdcs)
     (let ((env (new-env)))
       (interp-fdcs! fdcs env)
       (interp-stms/k! stms
                       (extend-env env)
                       (lambda (_)
                         (displayln "Done!"))
                       (lambda (_)
                         (report-return-outside-function))))]))

(define (interp-fdcs! fdcs env)
  (if (null? fdcs)
      (void)
      (match (car fdcs)
        [`(defun (,ret-type ,fname . ,arg-types) . ,body)
         ;(displayln env)
         (env-def! env fname (mk-func ret-type arg-types body env))
         ;(displayln env)
         ;(displayln (cdr env))
         (interp-fdcs! (cdr fdcs) env)])))

(define (interp-stms/k! stms env cont ret-cont)
  (if (null? stms)
      (cont (void))
      (let ((stm (car stms))
            (rest-stms (cdr stms)))
        (interp-stm/k! stm env
                       (lambda (_)
                         (interp-stms/k! rest-stms env cont ret-cont))
                       ret-cont))))

(define (interp-stm/k! stm env cont ret-cont)
  ;(displayln stm)
  ;(displayln env)
  (match stm
    [`(seq . ,ss)
     (interp-stms/k! ss (extend-env env) cont ret-cont)]
    [`(array-set! ,ea ,ei ,ev)
     (value-of/k ea
                 env
                 (lambda (arr)
                   (check-array-of-any arr)
                   (value-of/k ei
                               env
                               (lambda (i)
                                 (check-integer i)
                                 (value-of/k ev
                                             env
                                             (lambda (v)
                                               (array-set! arr (expval i) v)
                                               (cont (void))))))))]
    [`(if ,e ,s1 ,s2)
     (value-of/k e
                 env
                 (lambda (v)
                   (check-boolean v)
                   (if (expval v)
                       (interp-stm/k! s1 env cont ret-cont)
                       (interp-stm/k! s2 env cont ret-cont))))]
    [`(while ,e . ,ss)
     (value-of/k e
                 env
                 (lambda (v)
                   (check-boolean v)
                   (if (expval v)
                       (interp-stms/k!
                        ss
                        (extend-env env)
                        (lambda (_)
                          (interp-stm/k! stm env cont ret-cont))
                        ret-cont)
                       (cont (void)))))]
    [`(var ,var ,e)
     (value-of/k e
                 env
                 (lambda (val)
                   (env-def! env var val)
                   (cont (void))))]
    [`(set! ,var ,e)
     (value-of/k e
                 env
                 (lambda (val)
                   (env-set! env var val)
                   (cont (void))))]
    [`(output ,e)
     (value-of/k e
                 env
                 (lambda (val)
                   (output val)
                   (cont (void))))]
    [`(return ,e)
     (value-of/k e
                 env
                 (lambda (val)
                   (ret-cont val)))]
    [`(,rator . ,rands)
     (value-of-app-exp/k rator rands env cont)]))

(define (value-of/k expr env cont)
  ;(displayln expr)
  ;(displayln env)
  (match expr
    [(? integer? i) (cont (mk-integer i))]
    [(? boolean? b) (cont (mk-boolean b))]
    [(? symbol? s) (cont (apply-env env s))]
    [`(array ,en ,ev)
     (value-of/k en
                 env
                 (lambda (n)
                   (check-integer n)
                   (value-of/k ev
                               env
                               (lambda (v)
                                 (cont (mk-array (expval n) v))))))]
    [`(array-length ,e)
     (value-of/k e
                 env
                 (lambda (arr)
                   (check-array-of-any arr)
                   (cont (mk-integer (array-length arr)))))]
    [`(array-ref ,ea ,ei)
     (value-of/k ea
                 env
                 (lambda (arr)
                   (check-array-of-any arr)
                   (value-of/k ei
                               env
                               (lambda (i)
                                 (check-integer i)
                                 (cont (array-ref arr (expval i)))))))]
    [`(not ,e)
     (value-of/k e
                 env
                 (lambda (v)
                   (check-boolean v)
                   (cont (mk-boolean (not (expval v))))))]
    [`(,op ,e1 ,e2) #:when (memq op '(+ - * div mod = > < >= <=))
     (value-of/k
      e1
      env
      (lambda (v1)
        (check-integer v1)
        (value-of/k
         e2
         env
         (lambda (v2)
           (check-integer v2)
           (let ((i1 (expval v1))
                 (i2 (expval v2)))
             (cont (match op
                     ['+ (mk-integer (+ i1 i2))]
                     ['- (mk-integer (- i1 i2))]
                     ['* (mk-integer (* i1 i2))]
                     ['div (mk-integer (quotient i1 i2))]
                     ['mod (mk-integer (remainder i1 i2))]
                     ['= (mk-boolean (= i1 i2))]
                     ['> (mk-boolean (> i1 i2))]
                     ['< (mk-boolean (< i1 i2))]
                     ['>= (mk-boolean (>= i1 i2))]
                     ['<= (mk-boolean (<= i1 i2))])))))))]
    [`(,rator . ,rands)
     (value-of-app-exp/k rator rands env cont)]))

(define (value-of-app-exp/k rator rands env cont)
  (value-of/k
      rator
      env
      (lambda (func)
        (check-function func)
        (value-of-exps/k
         '()
         rands
         env
         (lambda (args)
           (check-sig func args)
           (call/k func args cont))))))

(define (value-of-exps/k vals exps env cont)
  (if (null? exps)
      (cont (reverse vals))
      (value-of/k (car exps)
                  env
                  (lambda (val)
                    (value-of-exps/k (cons val vals)
                                     (cdr exps)
                                     env
                                     cont)))))

; type
(define (mk-integer i) `(integer ,i))
(define (mk-boolean b) `(boolean ,b))
(define (mk-func ret-type arg-types body env)
  `(function ,ret-type ,arg-types ,body ,env))
(define (mk-array num val)
  (if (> num 0)
      `((array ,(exptype val)) ,(make-vector num val) ,num)
      (report-mk-array-invalid-length num)))

(define (array-set! arr i v)
  (check-type-eq (exparr-oftype arr) v)
  (check-index-range arr i)
  (vector-set! (expval arr) i v))

(define (array-ref arr i)
  (check-index-range arr i)
  (vector-ref (expval arr) i))

(define (check-index-range arr i)
  (let ((l (array-length arr)))
    (if (and (>= i 0) (< i l))
        (void)
        (report-index-out-of-range l i))))

(define (type-array? ty)
  (and (pair? ty) (eq? 'array (car ty))))

(define (expval v) (cadr v))
(define (exptype v) (car v))

(define (expfunc-ret v) (cadr v))
(define (expfunc-args  v) (caddr v))
(define (expfunc-body v) (cadddr v))
(define (expfunc-env v) (cadddr (cdr v)))

(define (array-length arr) (caddr arr))
(define (exparr-oftype arr) (cadar arr))

(define (check-type-eq ty val)
  (let ((v-type (exptype val)))
    (if (eq? ty v-type)
        (void)
        (report-type-checking-error ty v-type))))

(define (check-boolean v) (check-type-eq 'boolean v))
(define (check-integer v) (check-type-eq 'integer v))
(define (check-function v) (check-type-eq 'function v))
(define (check-array-of-any v)
  (let ((v-type (exptype v)))
    (if (type-array? v-type)
        (void)
        (report-type-checking-error 'array v-type))))

(define (value-of-type t)
  (match t
    ['void 'void]
    ['int 'integer]
    ['bool 'boolean]
    [`(array ,ty) `(array ,(value-of-type ty))]))

(define (check-set v1 v2)
  (check-type-eq (exptype v1) v2))

(define (check-sig func args)
  (let* ((args-sig (expfunc-args func))
         (numsig (length args-sig))
         (numargs (length args)))
    (if (= numsig numargs)
        (for-each check-type-eq
                  (map (lambda (d) (value-of-type (car d))) args-sig)
                  args)
        (report-argsnum-not-match numsig numargs))))

(define (check-ret-type ret-type given-val)
  (if (void? given-val)
      (check-type-eq ret-type '(void))
      (check-type-eq ret-type given-val)))

(define (call/k func args cont)
  (let ((args-sig (expfunc-args func))
        (ret-type (expfunc-ret func))
        (body (expfunc-body func))
        (ext-env (extend-env (expfunc-env func))))
    (for-each (lambda (var val)
                (env-def! ext-env var val))
              (map (lambda (d) (cadr d)) args-sig)
              args)
    (let ((ret-cont (lambda (v)  ; how to make this tail-call?
                      (check-ret-type (value-of-type ret-type) v)
                      (cont v))))
      (interp-stms/k! body ext-env ret-cont ret-cont))))

; environment

(define (new-env) (list (make-hash)))

(define (env-def! env var val)
  (hash-set! (car env) var val))

(define (env-set! env var val)  ; TODO: problem about colsure
  (if (null? env)
      (report-unbound-var var)
      (let ((scope (car env)))
        (if (hash-has-key? scope var)
            (let ((old-val (hash-ref scope var)))
              (check-set old-val val)
              (hash-set! scope var val))
            (env-set! (cdr env) var val)))))

(define (extend-env env)
  (cons (make-hash) env))

(define (apply-env env var)
  (if (null? env)
      (report-unbound-var var)
      (hash-ref (car env) var (lambda ()
                                (apply-env (cdr env) var)))))

; errors
(require "error.rkt")

(define (report-return-outside-function)
  (raise (error:return-outside-function
          "SyntaxError: 'return' outside function")))

(define (report-unbound-var var)
  (raise (error:unbound
          (format "Error: unbound variable ~a" var))))

(define (report-type-checking-error expected-type given-type)
  (raise (error:type-checking
          (format "TypeError: expected ~a but given ~a"
                  expected-type
                  given-type))))

(define (report-argsnum-not-match expected-num given-num)
  (raise (error:argsnum-not-match
          (format "Error: arity mismatch, expected ~a but given ~a"
                  expected-num
                  given-num))))

(define (report-index-out-of-range len idx)
  (raise (error:index-out-of-range
          (format "Error: index is out of range, range=[0, ~a], index=~a"
                  (- len 1)
                  idx))))

(define (report-mk-array-invalid-length len)
  (raise (error:mk-array-invalid-length
          (format "Error: cannot make an array of length ~a" len))))

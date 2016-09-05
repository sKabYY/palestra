#lang racket
(provide (combine-out cps-of-expr
                      interp-cps))

(define end-cont '(func (x) x))

(define (mkvar s) (string->symbol (string-append "#" s)))

(define (value-expr? expr)
  (or (symbol? expr)
      (number? expr)
      (boolean? expr)))

; This is copied from yinwang XD
(define (cps-of-expr expr)
  (letrec ((vk (mkvar "k"))
           (id (lambda (v) v))
           (ctx0 (lambda (v) `(,vk ,v)))
           (fv (let ((n 0))
                 (lambda ()
                   (set! n (+ n 1))
                   (mkvar (number->string n)))))
           (mk-cont (lambda (ctx)
                      (if (eq? ctx ctx0)
                          vk
                          (let ((u (fv)))
                            `(func (,u) ,(ctx u))))))
           (cps1
            (lambda (expr ctx)
              (match expr
                [(? value-expr? x) (ctx x)]
                [`(if ,test ,conseq ,alt)
                 (cps1 test
                       (lambda (t)
                         (if (memq ctx (list ctx0 id))
                             `(if ,t ,(cps1 conseq ctx) ,(cps1 alt ctx))
                             (let ((u (fv)))
                               `(let ((k (lambda (,u) ,(ctx u))))
                                  (if ,t ,(cps1 conseq ctx0) ,(cps1 alt ctx0)))))))]
                [`(func (,x) ,body)
                 (ctx `(func (,x) (func (,vk) ,(cps1 body ctx0))))]
                [`(- ,a ,b)
                 (cps1 a (lambda (v1)
                           (cps1 b (lambda (v2)
                                     `(-/k ,v1 ,v2 ,(mk-cont ctx))))))]
                [`(zero? ,e)
                 (cps1 e (lambda (v1)
                           `(zero?/k ,v1 ,(mk-cont ctx))))]
                [`(,rator ,rand)
                 (cps1 rator
                       (lambda (r)
                         (cps1 rand
                               (lambda (d)
                                 `((,r ,d) ,(mk-cont ctx))))))]))))
    (cps1 expr id)))

; === interp ===
(define (interp-cps expr)
  (value-of-cps expr (empty-env)))

(define (value-of-cps expr env)
  (match expr
    [(? symbol? s) (value-of-simple expr env)]
    [(? number? n) (value-of-simple expr env)]
    [(? boolean? b) (value-of-simple expr env)]
    [`(func (,a) ,body) (value-of-simple expr env)]
    [`(if ,e1 ,e2 ,e3)
     (if (value-of-simple e1 env)
         (value-of-cps e2 env)
         (value-of-cps e3 env))]
    [`(-/k ,e1 ,e2 ,k)
     (let ((v1 (value-of-simple e1 env))
           (v2 (value-of-simple e2 env))
           (vk (value-of-simple k env)))
       (apply-closure vk (- v1 v2)))]
    [`(zero?/k ,e ,k)
     (let ((ve (value-of-simple e env))
           (vk (value-of-simple k env)))
       (apply-closure vk (zero? ve)))]
    [`((,e1 ,e2) ,k)
     (let ((v1 (value-of-simple e1 env))
           (v2 (value-of-simple e2 env))
           (vk (value-of-simple k env)))
       (apply-closure (apply-closure v1 v2) vk))]
    [`(,k ,e)
     (let ((vk (value-of-simple k env))
           (ve (value-of-simple e env)))
       (apply-closure vk ve))]))

(define (value-of-simple expr env)
  (match expr
    [(? symbol? s) (apply-env env s)]
    [(? number? n) n]
    [(? boolean? b) b]
    [`(func (,a) ,body) (closure a body env)]
    [else (report-not-a-simple-expr expr)]))

; closure
(define (closure a body env)
  (lambda (v)
    (value-of-cps body (extend-env env a v))))

(define (apply-closure clo v) (clo v))

; environment
(define (empty-env)
  (lambda (s) (report-unbound-var s)))

(define (extend-env env a v)
  (lambda (s)
    (if (eqv? a s)
        v
        (apply-env env s))))

(define (apply-env env s) (env s))

; report error
(define (report-unbound-var s)
  (error "[Error] unbound var:" s))

(define (report-not-a-simple-expr s)
  (error "[Error]" s "is not a simple expr"))

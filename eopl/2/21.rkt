#lang eopl

(#%require "lib.rkt")

(define (report-no-binding-found search-var)
  (eopl:error "Unbound symbol:" search-var))

(define-datatype environment environment?
  (empty-env)
  (extend-env (enclosing-env environment?)
              (var symbol?)
              (val (lambda (v) #t))))

(define (apply-env env search-var)
  (cases environment env
    (empty-env () (report-no-binding-found search-var))
    (extend-env (enclosing-env var val)
                (if (eqv? search-var var)
                  val
                  (apply-env enclosing-env search-var)))))

(define (empty-env? env)
  (cases environment env
    (empty-env () #t)
    (else #f)))

(define (has-binding? env search-var)
  (cases environment env
    (empty-env () #f)
    (extend-env (enclosing-env var val)
                (if (eqv? search-var var)
                  #t
                  (has-binding? enclosing-env var)))))

(let* ((env (extend-env
              (extend-env
                (extend-env
                  (empty-env)
                  'x 1)
                'y 2)
              'x 3)))
  (displayln (empty-env? (empty-env)))
  (displayln (empty-env? env))
  (displayln (has-binding? env 'x))
  (displayln (has-binding? env 'y))
  (displayln (has-binding? env 'z))
  (displayln (apply-env env 'x))
  (displayln (apply-env env 'y))
  (displayln (apply-env env 'z)))

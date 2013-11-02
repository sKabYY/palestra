#lang eopl

(#%require "lib.rkt")

; Environment
; Constructors: empty-env, extend-env
; Observers: apply-env, empty-env?

(define (report-no-binding-found search-var)
  (eopl:error "Unbound symbol:" search-var))

(define (apply-env env var)
  ((car env) var))

(define (empty-env? env)
  ((cadr env)))

(define (empty-env)
  (list
    (lambda (var) (report-no-binding-found var))
    (lambda () #t)))

(define (extend-env env var val)
  (list
    (lambda (v)
      (if (eqv? v var)
        val
        (apply-env env v)))
    (lambda () #f)))

(let* ((env (extend-env
              (extend-env
                (extend-env
                  (empty-env)
                  'x 1)
                'y 2)
              'x 3)))
  (displayln (empty-env? (empty-env)))
  (displayln (empty-env? env))
  (displayln (apply-env env 'x))
  (displayln (apply-env env 'y))
  (displayln (apply-env env 'z)))

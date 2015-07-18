#lang eopl

(#%require "lib.rkt")

(define (report-no-binding-found search-var)
  (eopl:error "Unbound symbol:" search-var))

(define (apply-env env var)
  ((vector-ref env 0) var))

(define (empty-env? env)
  ((vector-ref env 1)))

(define (has-binding? env var)
  ((vector-ref env 2) var))

(define (empty-env)
  (vector
    (lambda (var) (report-no-binding-found var))
    (lambda () #t)
    (lambda (var) #f)))

(define (extend-env env var val)
  (vector
    (lambda (search-var)
      (if (eqv? search-var var)
        val
        (apply-env env search-var)))
    (lambda () #f)
    (lambda (search-var)
      (if (eqv? search-var var)
        #t
        (has-binding? env search-var)))))

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

#lang racket

(provide (combine-out type-check))

;;; errors

;;; type checking
(define (type-check pgm)
  (match pgm
    [`((main . ,stms) . ,fdcs)
     (let ((tenv (new-tenv)))
       (type-check-fdcs! fdcs tenv)
       (stms-type-check! stms (extend-tenv tenv)))]))

(define (type-check-fdcs! fdcs tenv)
  ; build tenv
  (for-each (lambda (fdc)
              (match fdc
                [`(defun (,ret-type ,fname . ,arg-types) . ,body)
                 (tenv-def! tenv
                            fname
                            (mk-function-type ret-type arg-types))])))
  (for-each (lambda (fdc)
              (match fdc
                [`(defun (,ret-type ,fname . ,arg-types) . ,body)
                 (let ((new-tenv (extend-tenv tenv)))
                   (for-each (lambda (arg-type)
                               (tenv-def! new-tenv
                                          (cadr arg-type)
                                          (typeval (car arg-type)))))
                   (body-type-check! (typeval ret-type) body new-tenv))]))))

(define (body-type-check! ret-type body tenv)
  ('todo))

(define (stms-type-check! stms tenv)
  (for-each (lambda (stm)
              (stm-type-check! stm tenv))
            stms))

(define (stm-type-check! stm tenv)
  (match stm
    [`(seq . ,stms)
     (stms-type-check! stms (extend-tenv tenv))]
    [`(array-set! ,ea ,ei ,ev)
     (check-integer (type-of ei tenv))
     (check-array-of (type-of ev tenv) (type-of ea tenv))]
    [`(if ,e ,s1 ,s2)
     (check-boolean (type-of e tenv))
     (stm-type-check! s1 tenv)
     (stm-type-check! s2 tenv)]
    [`(while ,e . ,ss)
     (check-boolean (type-of e tenv))
     (stms-type-check! ss (extend-tenv tenv))]
    [`(var ,var ,e)
     (tenv-def! tenv var (type-of e tenv))]
    [`(set! ,var ,e)
     (check-type-eq (apply-tenv tenv var) (type-of e))]
    [`(output ,e) (type-of e) (void)]
    [`(return ,e) 'todo]
    [`(,rator . ,rands) 'todo]))

(define (bin-op-type) 'todo)

(define (type-of expr tenv)
  (match expr
    [(? integer? i) (mk-integer-type)]
    [(? boolean? b) (mk-boolean-type)]
    [(? symbol? s) (apply-tenv tenv var)]
    [`(array, ,en ,ev)
     (check-integer (type-of en tenv))
     (mk-array-type (type-of ev tenv))]
    [`(array-length ,e)
     (check-array (type-of e tenv))
     (mk-integer-type)]
    [`(array-ref ,ea ,ei)
     (let ((ta (type-of ea tenv)))
       (check-array ta)
       (check-integer (type-of ei tenv))
       (elem-type-of-array ta))]
    [`(not ,e)
     (check-boolean (type-of e))
     (mk-boolean)]
    [`(,op ,e1 ,e2) #:when (memq op '(+ - * div mod = > < >= <=))
     (let ((t1 (type-of e1))
           (t2 (type-of e2)))
       'todo)]
    [`(,rator . ,rands) 'todo]))

(define (new-tenv) (list (make-hash)))

(define (extend-tenv env) (cons (make-hash) env))

(define (tenv-def! env var type)
  (hash-set! (car env) var type))

(define (apply-tenv env var)
  (if (null? env)
      (report-unbound-var var)
      (hash-ref (car env) var (lambda ()
                                (apply-tenv (cdr env) var)))))

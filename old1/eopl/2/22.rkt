#lang eopl

(#%require "lib.rkt")

(define (report-empty-stack-error)
  (eopl:error "The stack is empty"))

(define-datatype stack stack?
  (empty-stack)
  (push (stack stack?)
        (elem (lambda (v) #t))))

(define (pop st)
  (cases stack st
    (empty-stack ()
      (report-empty-stack-error))
    (push (stack elem)
      stack)))

(define (top st)
  (cases stack st
    (empty-stack ()
      (report-empty-stack-error))
    (push (stack elem)
      elem)))

(define (empty-stack? st)
  (cases stack st
    (empty-stack () #t)
    (push (stack elem) #f)))

(let* ((s1 (push (empty-stack) 1))
       (s2 (pop s1))
       (s3 (push s1 2))
       (s4 (pop s3)))
  (run-disp
    (top s1)
    (empty-stack? s2)
    (top s3)
    (eqv? s1 s4)))

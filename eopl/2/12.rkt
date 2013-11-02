#lang eopl

(#%require "lib.rkt")

; Constructors: empty-stack, push, pop
; Observers: top, empty-stack?

(define (report-empty-error)
  (eopl:error "The stack is empty"))

(define the-empty-stack
  (lambda (msg)
    (report-empty-error)))

(define (empty-stack) the-empty-stack)

(define (empty-stack? stack)
  (eqv? stack the-empty-stack))

(define (push stack elem)
  (lambda (msg)
    (cond ((eqv? msg 'top)
           elem)
          ((eqv? msg 'pop)
           stack)
          (eopl:error "Unknown msg -- PUSH" msg))))

(define (pop stack)
  (stack 'pop))

(define (top stack)
  (stack 'top))

(let* ((s1 (push (empty-stack) 1))
       (s2 (pop s1))
       (s3 (push s1 2))
       (s4 (pop s3)))
  (run-disp
    (top s1)
    (empty-stack? s2)
    (top s3)
    (eqv? s1 s4)))

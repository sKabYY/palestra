 (eval-when (compile load eval)
   (optimize-level 2)
   (case-sensitive #t)
 )

(load "compiler.ss")
(load "a14-wrapper.ss")   ; defines syntactic forms and procedures
(load "tests14.ss")

'(compiler-passes
 '(skip-used-name
   convert-complex-datum
   uncover-assigned
   purify-letrec
   convert-assignments
   ))

(compiler-passes
 '(skip-used-name
   convert-complex-datum
   uncover-assigned
   purify-letrec
   convert-assignments
   remove-anonymous-lambda
   uncover-free
   convert-closures
   optimize-known-call
   introduce-procedure-primitives
   lift-letrec
   normalize-context
   specify-representation
   remove-let
   verify-uil
   remove-complex-opera*
   impose-calling-conventions
   forward-locations
   uncover-frame-conflict
   pre-assign-frame
   assign-new-frame
   (iterate
    finalize-frame-locations
    select-instructions
    uncover-register-conflict
    assign-registers
    (break when everybody-home?)
    assign-frame)
   discard-call-live
   finalize-locations
   expose-mem-var
   expose-basic-blocks
   optimize-jumps
   flatten-program
   generate-x86-64))

(define (test-idx idx)
  (tracer #t)
  (let ([src (list-ref tests idx)])
    (pretty-print src)
    (test-one src #t)))

(define test
  (case-lambda
    [() (test-all)]
    [(idx) (test-idx idx)]))

;(trusted-passes #t)
;(trusted-passes '(remove-let))

;(check-final-output-only #t)
;(tracer '(remove-anonymous-lambda optimize-known-call))
(call/cc
 (lambda (k)
   (with-exception-handler
     (lambda (e)
       (tracer #t)
       (test-last #t)
       (k (void)))
     (lambda ()
       (test)))))

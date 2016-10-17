 (eval-when (compile load eval)
   (optimize-level 2)
   (case-sensitive #t)
 )

(load "compiler.ss")
(load "a9-wrapper.ss")   ; defines syntactic forms and procedures
(load "tests9.ss")

(compiler-passes
 '(remove-let
   verify-uil
   skip-used-name
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
   flatten-program
   generate-x86-64
   ))

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
;(tracer '(remove-let impose-calling-conventions forward-locations))
(test)

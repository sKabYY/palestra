(load "../lib/match.ss")
(load "../lib/helpers.ss")
;(load "helpers.ss")
(load "../lib/driver.ss")
(load "../lib/fmts.pretty")
(load "a8-wrapper.ss")
;(load "a7-wrapper.ss")
;(load "../lib/wrapper.ss")

(trusted-passes #t)
(reset-machine-state!)

(define src
  ((language-wrapper) 'impose-calling-conventions
   '(letrec ([f$1 (lambda () (set! rax fv0))])
      (new-frames
       ((nfv.1))
       (return-point rp$2
                     (begin
                       (set! nfv.1 123)
                       (f$1)))))))

(define src0
  ((language-wrapper) 'pre-assign-frame
   '(letrec ()
      (locate ((a.1 fv0) (b.2 fv0))
              (begin
                (set! a.1 1)
                (set! b.2 2)
                (set! rax a.1))))))

(define src1
  ((language-wrapper) 'pre-assign-frame
   '(letrec ()
      (begin
        (set! fv0 1)
        (set! rax fv0)))))

'((lambda (s)
    (pretty-print s)
    (pretty-print 'output:)
    (pretty-print ((game-eval) s)))
 src0)

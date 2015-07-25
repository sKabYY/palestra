#lang racket

(provide (all-defined-out))

(define color-on #t)
(define (color code) (if color-on (display code) (void)))
(define (begin-red) (color "\033[91m"))
(define (begin-green) (color "\033[92m"))
(define (begin-yellow) (color "\033[93m"))
(define (end-color) (color "\033[0m"))

(define (test interp cases)
  (define (iter cases total wrong)
    (if (null? cases)
        (printf "SUMMARY(wrong/total): ~a/~a.~%" wrong total)
        (let ((exp1 (car cases))
              (ans (cadr cases))
              (rest (cddr cases)))
          (begin-green)
          (pretty-print exp1)
          (begin-red)
          (let* ((ret (interp exp1))
                 (correct (eqv? ret ans)))
            (display ">> ")
            (pretty-print ret)
            (begin-yellow)
            (if correct
                (void)
                (printf "[WRONG!]ans=~a~%" ans))
            (end-color)
            (newline)
            (iter rest (+ total 1) (if correct wrong (+ wrong 1)))))))
  (iter cases 0 0))


(define cases
  `(

(lambda (f) (lambda (x) (lambda (y) ((f y) x)))) 0

a 0

(lambda (v) v) 0

(lambda (x) (lambda (y) (x y))) 0

(f a) 0

(lambda (f) (f a)) 0

((f x) y) 0

(g (s t)) 0

((lambda (v) v) x) 0

(lambda (x) ((lambda (y) y) x)) 0

    ))

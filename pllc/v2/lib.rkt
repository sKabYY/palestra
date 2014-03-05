#lang racket

(provide driver-loop
         interp-disp)

(define (driver-loop eval)
  (let ((input (read-input)))
    (if (eof-object? input)
      (begin (newline)(display 'Bye~)(newline))
      (let ((output (with-handlers
                      ((exn:fail?
                         (lambda (x)
                           (string-append "Error: " (exn-message x)))))
                      (eval input))))
        (display-output output)
        (driver-loop eval)))))

(define input-prompt "> ")
(define output-prompt "")

(define (read-input)
  (display input-prompt)(read))

(define (display-output output)
  (display output-prompt)(display output)(newline))

(define (begin-red) (display "\033[91m"))
(define (begin-green) (display "\033[92m"))
(define (begin-yellow) (display "\033[93m"))
(define (end-color) (display "\033[0m"))

(define (interp-disp interp sources)
  (define (iter prev-count sources wrong)
    (if (null? sources)
        (cons wrong prev-count)
        (let ([count (+ 1 prev-count)]
              [src (car sources)]
              [answer (cadr sources)])
          (display count) (displayln ":")
          (begin-green)
          (display src) (newline)
          (begin-red)
          (let ([output (interp src)])
            (display output) (newline)
            (end-color)
            (if (eqv? answer output)
                (iter count (cddr sources) wrong)
                (begin
                  (begin-yellow)
                  (printf "WRONG!!!!!!(answer: ~a  output: ~a)~%" answer output)
                  (end-color)
                  (iter count (cddr sources) (+ wrong 1))))))))
  (let ([summary (iter 0 sources 0)])
    (printf "SUMMARY(wrong/total): ~a/~a.~%" (car summary) (cdr summary))))

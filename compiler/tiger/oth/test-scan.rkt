#!/usr/bin/env racket
#lang racket

(require "../parsec.rkt")

(set-literal-types!
 ('z-token (lambda (s)
             (and (> (string-length s) 0)
                  (char=? (string-ref s 0) #\z)))))

(define scan-testcases
  '(
    "12"
    "(- 12 -13)"
    "(- 3 (- 2 1))"
    "zero?"
    "(zero? 0)"
    "#t"
    "#f"
    "#\\_"
    "(if (zero? o) 1 2)"
    "('()#f[#\\c])"
    "'(\"sss\");'(\"abc\")"
    "'(\"sss\")/*'(\"abc\")*/(1 2 3)"
    "'(\"sss\")/*'(\"abc\")(1 2 3)"
    ))

(set-comment-start! "/*")
(set-comment-end! "*/")
(let loop ([testcases scan-testcases])
  (if (null? testcases)
      (void)
      (let ((src (car testcases)))
        (printf "~n~a~n" src)
        (pretty-print (scan src))
        (loop (cdr testcases)))))

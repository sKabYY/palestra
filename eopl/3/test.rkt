#lang eopl

(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar-a
  '((statement
      ("{" (arbno statement ";") "}")
      compound-statement)
    (statement
      ("while" expression "do" statement)
      while-statement)
    (statement
      (identifier ":=" expression)
      assign-statement)
    (expression
      (identifier)
      var-exp)
    (expression
      ("(" expression "-" expression ")")
      diff-exp)))

(sllgen:make-define-datatypes scanner-spec-a grammar-a)
(eopl:pretty-print
  (sllgen:list-define-datatypes scanner-spec-a grammar-a))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-a grammar-a))

(display "\033[92m")

(newline)
;(eopl:pretty-print (scan&parse "{x := foo; y := bar; z := uu;}"))
;(eopl:pretty-print (scan&parse "{}"))
(define read-eval-print
  (sllgen:make-rep-loop
    "--> " (lambda (x) x)
    (sllgen:make-stream-parser scanner-spec-a grammar-a)))
(read-eval-print)

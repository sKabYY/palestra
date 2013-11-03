#lang eopl

(#%require "lib.rkt")

(define-datatype prefix-exp prefix-exp?
  (const-exp
    (num integer?))
  (diff-exp
    (operand1 prefix-exp?)
    (operand2 prefix-exp?)))

; <lst> is a list of integers or '-'
(define (parse lst)
  (define (diff-symbol? s)
    (and (symbol? s) (eqv? s '-)))
  (if (null? lst)
    (eopl:error "Get nil")
    (let ((e (car lst)))
      (cond
        ((integer? e)
         (pair
           (const-exp e)
           (cdr lst)))
        ((diff-symbol? e)
         (let* ((pair1 (parse (cdr lst)))
                (exp1 (pair-fst pair1))
                (left1 (pair-snd pair1))
                (pair2 (parse left1))
                (exp2 (pair-fst pair2))
                (left2 (pair-snd pair2)))
           (pair
             (diff-exp exp1 exp2)
             left2)))
        (else
          (eopl:error "Unknown symbol" e))))))

(run-disp
  (parse '(- - 3 2 - 4 - 12 7))
  (parse '(- 3 2 1)))

#lang eopl

; Program ::= Expression
;
; Expression ::= Number
;            ::= primitive-procedure({Expression}*)
;            ::= if Expression then Expression else Expression
;            ::= Identifier
;            ::= let {Identifier = Expression}* in Expression
;
; primitive-procedure: minus, diff(-), addition(+), ,multiplication(*),
;                      quotient, remainder,
;                      zero?, equal?, greater?, less?,
;                      cons, car, cdr, null?, list
;
; global value: emptylist

(define-datatype program program?
  (a-program
    (exp1 expression?)))

(define (list-of-type1 pred val)
  (and (list? val)
       (or (null? val)
           (and (pred (car val))
                (list-of-type1 pred (cdr val))))))
(define (list-of-type pred)
  (lambda (val) (list-of-type1 pred val)))

(define identifier? symbol?)
(define primitive? procedure?)

(define-datatype expression expression?
  (number-exp
    (num number?))
  (primitive-exp
    (proc primitive?)
    (args (list-of-type expression?)))
  (if-exp
    (exp1 expression?)
    (exp2 expression?)
    (exp3 expression?))
  (var-exp
    (var identifier?))
  (let-exp
    (vars (list-of-type identifier?))
    (exps (list-of-type expression?))
    (body expression?)))

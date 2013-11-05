#lang eopl

; Program ::= Expression
;
; Expression ::= Number
;            ::= primitive-procedure({Expression}*)
;            ::= if Expression then Expression else Expression
;            ::= Identifier
;            ::= let {Identifier = Expression}* in Expression
;
; ExpVal: Number, Boolean, List
; DenVal = ExpVal
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

(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (list-val
    (lst list?)))

(define (report-expval-extractor-error type val)
  (error "Type error:" val type))

(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (report-expval-extractor-error 'num val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (report-expval-extractor-error 'bool val))))

(define (expval->list val)
  (cases expval val
    (list-val (lst) lst)
    (else (report-expval-extractor-error 'list val))))

#lang eopl

; Program ::= Expression
;
; Expression ::= Number
;            ::= primitive-procedure({Expression}*(,))
;            ::= proc ({Identifier}*(,)) Expression
;            ::= (Expression {Expression}*)
;            ::= if Expression then Expression else Expression
;            ::= Identifier
;            ::= let {Identifier = Expression}*(,) in Expression
;
; ExpVal: Number, Boolean, List, Proc
; DenVal = ExpVal
;
; primitive-procedures: minus, diff(-), addition(+), ,multiplication(*),
;                       quotient, remainder,
;                       zero?, equal?, greater?, less?,
;                       cons, car, cdr, null?, list
;
; global value: emptylist

(define scanner-spec
  '((white-sp (whitespace) skip)
    (commont ("#" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (identifier
      ((or letter)
       (arbno
         (or "-" "_" letter digit))) symbol)))

(define grammar-spec
  '((program
      (expression)
      a-program)
    (expression
      (number)
      number-exp)
    (expression
      (identifier)
      var-exp)
    (expression
      (primitive
        "(" (separated-list expression ",") ")")
      apply-primitive-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")"
       expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      call-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
    (expression
      ("let" (separated-list identifier "=" expression ",") "in" expression)
      let-exp)
    (primitive ("+") add-prim)
    (primitive ("-") diff-prim)
    (primitive ("*") mult-prim)
    (primitive ("minus") minus-prim)
    (primitive ("quotient") quotient-prim)
    (primitive ("remainder") remainder-prim)
    (primitive ("zero?") zero?-prim)
    (primitive ("equal?") equal?-prim)
    (primitive ("greater?") greater?-prim)
    (primitive ("less?") less?-prim)
    (primitive ("cons") cons-prim)
    (primitive ("car") car-prim)
    (primitive ("cdr") cdr-prim)
    (primitive ("null?") null?-prim)
    (primitive ("list") list-prim)))

(sllgen:make-define-datatypes
  scanner-spec grammar-spec)

(define scan&parse
  (sllgen:make-string-parser
    scanner-spec grammar-spec))

(eopl:pretty-print
  (sllgen:list-define-datatypes
    scanner-spec grammar-spec))
;
;(define test-parse
;  (sllgen:make-rep-loop
;    "> " (lambda (x) x)
;    (sllgen:make-stream-parser
;      scanner-spec grammar-spec)))
;
;(test-parse)

; expval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (list-val
    (lst list0?))
  (proc-val
    (proc proc?)))

(define (report-expval-extractor-error type val)
  (eopl:error "Type error:" val type))

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

(define (expval->proc val)
  (cases expval val
    (proc-val (proc) proc)
    (else (report-expval-extractor-error 'proc val))))

(define (minus val)
  (cases expval val
    (num-val (num) (num-val (- num)))
    (else report-expval-extractor-error 'num val)))

; list ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype list0 list0?
  (empty-list)
  (non-empty-list
    (first expval?)
    (rest expval?)))

(define the-empty-list (list-val (empty-list)))

(define (report-null-error)
  (eopl:error "() is not a pair"))

(define (cons0 first rest)
  (list-val (non-empty-list first rest)))

(define (car0 lst)
  (cases list0 (expval->list lst)
    (empty-list () report-null-error)
    (non-empty-list (first rest) first)))

(define (cdr0 lst)
  (cases list0 (expval->list lst)
    (empty-list () report-null-error)
    (non-empty-list (first rest) rest)))

(define (empty-list? lst)
  (cases expval lst
    (list-val (l)
      (if (list0? l)
        (cases list0 l
          (empty-list () (bool-val #t))
          (else (bool-val #f)))
        (bool-val #f)))
    (else #f)))

(define (mklist . elems)
  (if (null? elems)
    the-empty-list
    (cons0 (car elems) (apply mklist (cdr elems)))))

; procedure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype proc0 proc?
  (procedure
    (list-of-var (list-of symbol?))
    (body expression?)
    (env environment?)))

(define (apply-proc proc list-of-val)
  (cases proc0 proc
    (procedure (list-of-var body env)
      (let ((nvars (length list-of-var))
            (nvals (length list-of-val)))
        (if (= nvars nvals)
          (value-of
            body
            (extend-all-env env list-of-var list-of-val))
          (report-arguments-not-match
            list-of-var list-of-val))))))

(define (report-arguments-not-match vars vals)
  (eopl:error "args not match" vars vals))

; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype environment environment?
  (empty-env)
  (extend-env
    (enclosing-environment environment?)
    (var symbol?)
    (val expval?)))

(define (report-unbound-var search-var)
  (eopl:error "Unbound variable" search-var))

(define (extend-all-env env list-of-symbol list-of-exp)
  ; assert (= (length list-of-symbol) (length list-of-exp))
  (if (null? list-of-symbol)
    env
    (extend-all-env
      (extend-env env (car list-of-symbol) (car list-of-exp))
      (cdr list-of-symbol)
      (cdr list-of-exp))))

(define (apply-env env search-var)
  (cases environment env
    (empty-env () (report-unbound-var search-var))
    (extend-env (enclosing-env var val)
      (if (eqv? var search-var)
        val
        (apply-env enclosing-env search-var)))))

; value-of ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (value-of exp1 (init-env)))))

(define (value-of exp env)
  (define (value-of-list-of-exp list-of-exp)
    (map (lambda (e) (value-of e env)) list-of-exp))
  (cases expression exp
    (number-exp (number) (num-val number))
    (var-exp (identifier) (apply-env env identifier))
    (apply-primitive-exp (prim list-of-exp)
      (apply-primitive
        prim
        (value-of-list-of-exp list-of-exp)))
    (proc-exp (list-of-var body)
      (proc-val (procedure list-of-var body env)))
    (call-exp (rator rands)
      (apply-proc
        (expval->proc
          (value-of rator env))
        (value-of-list-of-exp rands)))
    (if-exp (exp1 exp2 exp3)
      (if (expval->bool (value-of exp1 env))
        (value-of exp2 env)
        (value-of exp3 env)))
    (let-exp (list-of-symbol list-of-exp body)
      (let ((new-env (extend-all-env
                       env
                       list-of-symbol
                       (value-of-list-of-exp list-of-exp))))
        (value-of body new-env)))))

(define (init-env)
  (extend-env
    (empty-env)
    'emptylist
    the-empty-list))

(define (apply-primitive prim list-of-exp)
  (cases primitive prim
    (add-prim ()
      (num-val (apply + (map expval->num list-of-exp))))
    (diff-prim ()
      (num-val (apply - (map expval->num list-of-exp))))
    (mult-prim ()
      (num-val (apply * (map expval->num list-of-exp))))
    (minus-prim () (apply minus list-of-exp))
    (quotient-prim ()
      (num-val (apply quotient (map expval->num list-of-exp))))
    (remainder-prim ()
      (num-val (apply remainder (map expval->num list-of-exp))))
    (zero?-prim ()
      (bool-val (apply zero? (map expval->num list-of-exp))))
    (equal?-prim ()
      (bool-val (apply = (map expval->num list-of-exp))))
    (greater?-prim ()
      (bool-val (apply > (map expval->num list-of-exp))))
    (less?-prim ()
      (bool-val (apply < (map expval->num list-of-exp))))
    (cons-prim () (apply cons0 list-of-exp))
    (car-prim () (apply car0 list-of-exp))
    (cdr-prim () (apply cdr0 list-of-exp))
    (null?-prim () (apply empty-list? list-of-exp))
    (list-prim () (apply mklist list-of-exp))))

; read-eval-print ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-eval-print
  (sllgen:make-rep-loop
    "> " value-of-program
    (sllgen:make-stream-parser
      scanner-spec grammar-spec)))

;(read-eval-print)

(#%require "lib.rkt")
(interp-disp
  (lambda (src) (value-of-program (scan&parse src)))
  (list

"42"
"+(1, 2)"
"-(1, 2)"
"*(2, 3)"
"quotient(5, 2)"
"remainder(12, 7)"
"minus(2)"
"zero?(0)"
"zero?(1)"
"equal?(1, 1)"
"equal?(1, 2)"
"greater?(1, 2)"
"greater?(2, 2)"
"greater?(3, 2)"
"less?(1, 2)"
"less?(2, 2)"
"less?(3, 2)"
"list(1, 2, 3)"
"cons(0, cons(1, emptylist))"
"cons(0, list(1, 2, 3))"
"car(list(1, 2, 3))"
"cdr(list(1, 2, 3))"
"null?(list(1, 2))"
"null?(emptylist)"
"if zero?(0) then +(1, 1) else -(1, 1)"
"if zero?(1) then +(1, 1) else -(1, 1)"
"let x = 12144 in x"
"let x = 12, y = 3 in +(x, y)"
"let x = 12 in let x = 13 in *(x, 2)"
"(proc (x, y) +(x, y) 2 3)"

"let f = proc (x) -(x, 11)
in (f (f 77))"

"(proc (f) (f (f 77))
proc (x) -(x, 11))"

"let x = 200
in let f = proc (z) -(z, x)
   in let x = 100
      in let g = proc (z) -(z, x)
         in -((f 1), (g 1))"

;"let double = proc (x) if zero?(x) then 0 else +(2, (double -(x, 1)))
;in (double 12)"

"let makemult = proc (maker)
                proc (x)
                 if zero?(x)
                 then 0
                 else +(4, ((maker maker) -(x, 1)))
in let times4 = proc (x) ((makemult makemult) x)
   in (times4 3)"

"let addx = proc (y) proc (x) +(x, y)
in let add1 = (addx 1)
   in (add1 12)"

"let Y = proc (f)
         (proc (u) (u u)
          proc (x)
           (f proc (v) ((x x) v)))
in let double = (Y proc (d)
                    proc (x)
                     if zero?(x)
                     then 0
                     else +(2, (d -(x, 1))))
   in (double 12)"

    ))

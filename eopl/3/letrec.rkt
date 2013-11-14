#lang eopl

(#%provide interp)
(define (interp src) (value-of-program (scan&parse src)))

; Program ::= Expression
;
; Expression ::= Number
;            ::= primitive-procedure({Expression}*(,))
;            ::= proc ({Identifier}*(,)) Expression
;            ::= (Expression {Expression}*)
;            ::= if Expression then Expression else Expression
;            ::= Identifier
;            ::= let {Identifier = Expression}*(,) in Expression
;            ::= letrec {Identifier ({Identifier}*(,)) = Expression}*(,) in Expression
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
    (expression
      ("letrec"
       (separated-list
         identifier
         "(" (separated-list identifier ",") ")" "=" expression
         ",")
       "in" expression)
      letrec-exp)
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

(define (proc-val-procedure args body env)
  (proc-val (procedure args body env)))

(define (apply-proc proc list-of-val)
  (cases proc0 proc
    (procedure (list-of-var body env)
      (let ((nvars (length list-of-var))
            (nvals (length list-of-val)))
        (if (= nvars nvals)
          (value-of
            body
            (extend-env env list-of-var list-of-val))
          (report-arguments-not-match
            list-of-var list-of-val))))))

(define (report-arguments-not-match vars vals)
  (eopl:error "args not match" vars vals))

; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype environment environment?
  (empty-env)
  (extend-env
    ; assert (= (length list-of-symbol) (length list-of-exp))
    (enclosing-environment environment?)
    (list-of-var (list-of symbol?))
    (list-of-val (list-of expval?)))
  (extend-env-rec
    ; assert (= (length list-of-name) (length list-of-args) (length list-of-body))
    (enclosing-environment environment?)
    (list-of-name (list-of symbol?))
    (list-of-args (list-of (list-of symbol?)))
    (list-of-body (list-of expression?))))

(define (report-unbound-var search-var)
  (eopl:error "Unbound variable" search-var))

(define (search-first search-var constructor enclosing-env vars . rest)
  (if (null? vars)
    (apply-env enclosing-env search-var)
    (if (eqv? search-var (car vars))
      (apply constructor (map car rest))
      (apply search-first
             search-var
             constructor
             enclosing-env (cdr vars) (map cdr rest)))))

(define (apply-env env search-var)
  (define (search-vars enclosing-env vars vals)
    (search-first search-var (lambda (x) x) enclosing-env vars vals))
  (define (search-rec-vars enclosing-env
                           list-of-name list-of-args list-of-body)
    (search-first
      search-var
      (lambda (args body) (proc-val-procedure args body env))
      enclosing-env
      list-of-name list-of-args list-of-body))
  (cases environment env
    (empty-env () (report-unbound-var search-var))
    (extend-env (enclosing-env vars vals)
      (search-vars enclosing-env vars vals))
    (extend-env-rec (enclosing-env
                      list-of-name
                      list-of-args
                      list-of-body)
      (search-rec-vars enclosing-env
                       list-of-name list-of-args list-of-body))))

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
      (proc-val-procedure list-of-var body env))
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
      (let ((new-env (extend-env
                       env
                       list-of-symbol
                       (value-of-list-of-exp list-of-exp))))
        (value-of body new-env)))
    (letrec-exp (list-of-name list-of-args list-of-body letrec-body)
      (let ((new-env (extend-env-rec
                       env
                       list-of-name
                       list-of-args
                       list-of-body)))
        (value-of letrec-body new-env)))))

(define (init-env)
  (extend-env
    (empty-env)
    (list 'emptylist)
    (list the-empty-list)))

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

#lang eopl

(#%provide interp)
(define (interp src)
  (value-of-program
    (translation-of-program
      (scan&parse src))))

; Program ::= Expression
;
; Expression ::= Number
;            ::= Identifier
;            ::= primitive-procedure({Expression}*(,))
;            ::= proc ({Identifier}*(,)) Expression
;            ::= (Expression {Expression}*)
;            ::= if Expression then Expression else Expression
;            ::= let {Identifier = Expression}*(,) in Expression
;            ::= letrec {Identifier ({Identifier}*(,)) = Expression}*(,) in Expression
;            ;% trick for defining datatype
;            ::= %lexref number number
;            ::= %lexproc Expression
;            ::= %let {Expression}*(,) in Expression
;            ::= %letrec {Expression}*(,) in Expression
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
    (expression
      ("%lexref" number number)
      nameless-var-exp)
    (expression
      ("%lexproc" expression)
      nameless-proc-exp)
    (expression
      ("%let" (separated-list expression ",") "in" expression)
      nameless-let-exp)
    (expression
      ("%letrec" (separated-list expression ",") "in" expression)
      nameless-letrec-exp)
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

;(eopl:pretty-print
;  (sllgen:list-define-datatypes
;    scanner-spec grammar-spec))
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
    (body expression?)
    (env environment?)))

(define (proc-val-procedure body env)
  (proc-val (procedure body env)))

(define (apply-proc proc list-of-val)  ; TODO  check number of args
  (cases proc0 proc
    (procedure (body env)
      (value-of
        body
        (extend-env env list-of-val)))))

; environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (empty-senv) '())

(define (extend-senv senv vars) (cons vars senv))

(define (apply-senv senv search-var)
  (define (search-senv addr senv)
    (if (null? senv)
      (report-unbound-symbol search-var)
      (search-frame addr 0 (car senv) (cdr senv))))
  (define (search-frame addr offset frame next)
    (if (null? frame)
      (search-senv (+ addr 1) next)
      (if (eqv? search-var (car frame))
        (list addr offset)
        (search-frame addr (+ offset 1) (cdr frame) next))))
  (search-senv 0 senv))

(define (report-unbound-symbol s)
  (eopl:error "unbound symbol:" s))

(define (environment? v) (list? v))

(define (empty-env) '())

(define (extend-env env vals)
  (cons
    (vector vals)
    env))

(define (extend-env-rec env bodies)
  (let* ((vec (make-vector 1))
         (new-env (cons vec env)))
    (vector-set!
      vec 0
      (map (lambda (b) (proc-val-procedure b new-env)) bodies))
    new-env))

(define (apply-env env addr offset)
  (let ((frame (vector-ref (list-ref env addr) 0)))
    (list-ref frame offset)))

(define (init-senv)
  (extend-senv
    (empty-senv)
    (list 'emptylist)))

(define (init-env)
  (extend-env
    (empty-env)
    (list the-empty-list)))

; translation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (translation-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (a-program
        (translation-of exp1 (init-senv))))))

(define (translation-of exp senv)

  (define (mk-nl-var-exp loi)
    (apply nameless-var-exp loi))

  (define (translation-of-list-of-exp list-of-exp senv)
    (map (lambda (e) (translation-of e senv)) list-of-exp))

  (define (translation-of-list-of-exp-each-senv list-of-exp list-of-senv)
    (map translation-of list-of-exp list-of-senv))

  (define (mk-list-of-senv list-of-args senv)
    (map (lambda (args) (extend-senv senv args)) list-of-args))

  (cases expression exp
    (number-exp (number) (number-exp number))
    (var-exp (var)
      (mk-nl-var-exp (apply-senv senv var)))
    (apply-primitive-exp (prim list-of-exp)
      (apply-primitive-exp
        prim
        (translation-of-list-of-exp list-of-exp senv)))
    (proc-exp (list-of-var body)
      (nameless-proc-exp
        (translation-of
          body
          (extend-senv senv list-of-var))))
    (call-exp (rator rands)
      (call-exp
        (translation-of rator senv)
        (translation-of-list-of-exp rands senv)))
    (if-exp (exp1 exp2 exp3)
      (if-exp
        (translation-of exp1 senv)
        (translation-of exp2 senv)
        (translation-of exp3 senv)))
    (let-exp (list-of-symbol list-of-exp body)
      (nameless-let-exp
        (translation-of-list-of-exp list-of-exp senv)
        (translation-of body (extend-senv senv list-of-symbol))))
    (letrec-exp (list-of-name list-of-args list-of-body letrec-body)
      (let ((new-senv (extend-senv senv list-of-name)))
        (nameless-letrec-exp
          (translation-of-list-of-exp-each-senv
            list-of-body
            (mk-list-of-senv list-of-args new-senv))
          (translation-of letrec-body new-senv))))
    (else
      (report-invalid-source-expression exp))))

(define (report-invalid-source-expression exp)
  (eopl:error "invalid-source-expression" exp))

; value ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (value-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (value-of exp1 (init-env)))))

(define (value-of exp env)
  (define (value-of-list-of-exp list-of-exp)
    (map (lambda (e) (value-of e env)) list-of-exp))
  (cases expression exp
    (number-exp (number) (num-val number))
    (nameless-var-exp (addr offset) (apply-env env addr offset))
    (apply-primitive-exp (prim list-of-exp)
      (apply-primitive
        prim
        (value-of-list-of-exp list-of-exp)))
    (nameless-proc-exp (body)
      (proc-val-procedure body env))
    (call-exp (rator rands)
      (apply-proc
        (expval->proc
          (value-of rator env))
        (value-of-list-of-exp rands)))
    (if-exp (exp1 exp2 exp3)
      (if (expval->bool (value-of exp1 env))
        (value-of exp2 env)
        (value-of exp3 env)))
    (nameless-let-exp (list-of-exp body)
      (let ((new-env (extend-env
                       env
                       (value-of-list-of-exp list-of-exp))))
        (value-of body new-env)))
    (nameless-letrec-exp (list-of-body letrec-body)
      (let ((new-env (extend-env-rec
                       env
                       list-of-body)))
        (value-of letrec-body new-env)))
    (else
      (report-invalid-translated-expression exp))))

(define (report-invalid-translated-expression exp)
  (eopl:error "invalid-translated-expression" exp))

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

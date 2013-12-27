#lang eopl

(#%provide interp
           cps-translate
           cps-program->list
           value-of-program/k)
(define (interp src) (value-of-program (scan&parse src)))
(define (cps-translate src) (cps-of-program (scan&parse src)))

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
    (proc proc?))
  (cps-proc-val
    (proc cps-proc?)))

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

(define (expval->cps-proc val)
  (cases expval val
    (cps-proc-val (proc) proc)
    (else (report-expval-extractor-error 'cps-proc val))))

(define (minus val)
  (cases expval val
    (num-val (num) (num-val (- num)))
    (else report-expval-extractor-error 'num val)))

; list ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype list0 list0?
  (empty-list)
  (non-empty-list
    (head expval?)
    (tail expval?)))

(define the-empty-list (list-val (empty-list)))

(define (report-null-error)
  (eopl:error "() is not a pair"))

(define (cons0 head tail)
  (list-val (non-empty-list head tail)))

(define (car0 lst)
  (cases list0 (expval->list lst)
    (empty-list () report-null-error)
    (non-empty-list (head tail) head)))

(define (cdr0 lst)
  (cases list0 (expval->list lst)
    (empty-list () report-null-error)
    (non-empty-list (head tail) tail)))

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
(define-datatype frame frame?
  (frame-vec (vec vector?)))

(define (a-frame vars vals)
  (frame-vec (vector vars vals)))

(define (empty-frame)
  (a-frame '() '()))

(define (frame-set! frm vars vals)
  (cases frame frm
    (frame-vec (vec)
      (vector-set! vec 0 vars)
      (vector-set! vec 1 vals))))

(define (frame-vars frm)
  (cases frame frm
    (frame-vec (vec) (vector-ref vec 0))))

(define (frame-vals frm)
  (cases frame frm
    (frame-vec (vec) (vector-ref vec 1))))

(define-datatype environment environment?
  (empty-env)
  (extend-env-frame
    (enclosing-environemt environment?)
    (frm frame?)))

(define (extend-env env vars vals)
  ; assert (= (length list-of-symbol) (length list-of-exp))
  (extend-env-frame env (a-frame vars vals)))

(define (extend-env-rec env list-of-name list-of-args list-of-body)
  ; assert (= (length list-of-name) (length list-of-args) (length list-of-body))
  (let* ((frm (empty-frame))
         (new-env (extend-env-frame env frm))
         (mk-proc (lambda (args body)
                    (proc-val-procedure args body new-env))))
    (frame-set! frm
                list-of-name
                (map mk-proc list-of-args list-of-body))
    new-env))

(define (apply-env env search-var)
  (define (search-env env)
    (cases environment env
      (empty-env () (report-unbound-var search-var))
      (extend-env-frame (enclosing-env frm)
        (search-frame (frame-vars frm) (frame-vals frm) enclosing-env))))
  (define (search-frame vars vals next-env)
    (if (null? vars)
      (search-env next-env)
      (if (eqv? search-var (car vars))
        (car vals)
        (search-frame (cdr vars) (cdr vals) next-env))))
  (search-env env))

(define (report-unbound-var search-var)
  (eopl:error "Unbound variable" search-var))

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

; CPS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Program ::= TfExp
;
; SimpleExp ::= Number
;           ::= Identifier
;           ::= primitive-procedure({SimpleExp}*(,))
;           ::= proc ({Identifier}*(,)) TfExp
;
; TfExp ::= SimpleExp
;       ::= let {Identifier = SimpleExp}*(,) in TfExp
;       ::= letrec {Identifier ({Identifier}*(,)) = TfExp}*(,) in TfExp
;       ::= if SimpleExp then TfExp else TfExp
;       ::= (SimpleExp {SimpleExp}*)

(define-datatype cps-program cps-program?
  (cps-a-program (tf-exp tf-exp?)))

(define-datatype simple-exp simple-exp?
  (cps-end-cont)
  (cps-number-exp (number number?))
  (cps-var-exp (var symbol?))
  (cps-apply-primitive-exp
    (prim primitive?)
    (rands (list-of simple-exp?)))
  (cps-proc-exp
    (vars (list-of symbol?))
    (body tf-exp?)))

(define-datatype tf-exp tf-exp?
  (simple-exp->exp (simple-exp1 simple-exp?))
  (cps-let-exp
    (vars (list-of symbol?))
    (vals (list-of simple-exp?))
    (body tf-exp?))
  (cps-letrec-exp
    (p-names (list-of symbol?))
    (b-varss (list-of (list-of symbol?)))
    (p-bodies (list-of tf-exp?))
    (body tf-exp?))
  (cps-if-exp
    (simple1 simple-exp?)
    (body1 tf-exp?)
    (body2 tf-exp?))
  (cps-call-exp
    (rator simple-exp?)
    (rands (list-of simple-exp?))))

(define end-cont (cps-end-cont))

(define (cps-end-cont? cont)
  (cases simple-exp cont
    (cps-end-cont () #t)
    (else #f)))

(define (cps-of-program pgm)
  (cases program pgm
    (a-program (exp1)
      (cps-a-program (cps-of-exp exp1 end-cont)))))

(define (make-send-to-cont cont simple-exp)
  (if (cps-end-cont? cont)
      (simple-exp->exp simple-exp)
      (cps-call-exp cont (list simple-exp))))

(define cont-v '/k)

(define (tmp-var n)
  (string->symbol (string-append "/" (number->string n))))

(define (all pred ls)
  (if (null? ls)
      #t
      (if (pred (car ls))
          (all pred (cdr ls))
          #f)))

(define (inp-exp-simple? exp1)
  (cases expression exp1
    (number-exp (number) #t)
    (var-exp (var) #t)
    (proc-exp (vars body) #t)
    (apply-primitive-exp (prim exps)
      (all inp-exp-simple? exps))
    (else #f)))

(define (cps-of-simple exp1)
  (cases expression exp1
    (number-exp (number) (cps-number-exp number))
    (var-exp (var) (cps-var-exp var))
    (proc-exp (vars body)
      (cps-proc-exp (cons cont-v vars)
                    (cps-of-exp body (cps-var-exp cont-v))))
    (apply-primitive-exp (prim exps)
      (cps-apply-primitive-exp prim
                               (map cps-of-simple exps)))
    (else 'error-TODO)))

(define (cps-of-exps exps builder)
  (define n 0)
  (define (refresh-var)
    (set! n (+ n 1))
    (tmp-var n))
  (define (iter vars exps)
    (if (null? exps)
        (builder (reverse vars))
        (let ((head (car exps))
              (tail (cdr exps))
              (var (refresh-var)))
          (if (inp-exp-simple? head)
              (iter (cons (cps-of-simple head) vars) tail)
              (cps-of-exp head
                          (cps-proc-exp (list var)
                                        (iter (cons (cps-var-exp var) vars)
                                              tail)))))))
  (iter '() exps))

(define (cps-of-exp exp1 cont)
  (cases expression exp1
    (number-exp (number)
      (make-send-to-cont cont (cps-number-exp number)))
    (var-exp (var)
      (make-send-to-cont cont (cps-var-exp var)))
    (proc-exp (vars body)
      (make-send-to-cont cont
                         (cps-proc-exp (cons cont-v vars)
                                       (cps-of-exp body (cps-var-exp cont-v)))))
    (apply-primitive-exp (prim exps)
      (cps-of-exps exps
                   (lambda (simple-exps)
                     (make-send-to-cont cont
                                        (cps-apply-primitive-exp prim simple-exps)))))
    (call-exp (rator rands)
      (cps-of-exps (cons rator rands)
                   (lambda (simple-exps)
                     (cps-call-exp (car simple-exps)
                                   (cons cont (cdr simple-exps))))))
    (if-exp (exp1 exp2 exp3)
      (cps-of-exps (list exp1)
                   (lambda (simple-exps)
                     (cps-if-exp (car simple-exps)
                                 (cps-of-exp exp2 cont)
                                 (cps-of-exp exp3 cont)))))
    (let-exp (vars exps body)
      (cps-of-exps exps
                   (lambda (simple-exps)
                     (cps-let-exp vars simple-exps (cps-of-exp body cont)))))
    (letrec-exp (p-names b-varss p-bodies body)
      (let ((cps-b-varss (map (lambda (vars)
                                (cons cont-v vars))
                              b-varss))
            (cps-p-bodies (map (lambda (p-body)
                                 (cps-of-exp p-body (cps-var-exp cont-v)))
                               p-bodies)))
        (cps-letrec-exp p-names
                        cps-b-varss
                        cps-p-bodies
                        (cps-of-exp body cont))))))

(define (zip l1 l2) (map (lambda (x y) (list x y)) l1 l2))
(define (s>> sexps) (map simple-exp->list sexps))

(define (cps-program->list cps-pgm)
  (cases cps-program cps-pgm
    (cps-a-program (cps-exp)
      (cps-exp->list cps-exp))))

(define (simple-exp->list sexp)
  (cases simple-exp sexp
    (cps-number-exp (number) number)
    (cps-var-exp (var) var)
    (cps-apply-primitive-exp (prim rands)
      (cons (primitive->symbol prim) (s>> rands)))
    (cps-proc-exp (vars body)
      (list
        'lambda vars (cps-exp->list body)))
    (cps-end-cont () 'end-cont)))

(define (cps-exp->list cps-exp)
  (cases tf-exp cps-exp
    (simple-exp->exp (sexp)
      (simple-exp->list sexp))
    (cps-let-exp (vars vals body)
     (list
       'let
       (zip vars (s>> vals))
       (cps-exp->list body)))
    (cps-letrec-exp (p-names b-varss p-bodies body)
      (list
        'letrec
        (zip p-names
             (map (lambda (b-vars p-body)
                    (list 'lambda b-vars (cps-exp->list p-body)))
                  b-varss p-bodies))
        (cps-exp->list body)))
    (cps-if-exp (simple1 body1 body2)
      (list
        'if (simple-exp->list simple1)
        (cps-exp->list body1)
        (cps-exp->list body2)))
    (cps-call-exp (rator rands)
      (cons (simple-exp->list rator) (s>> rands)))))

(define (primitive->symbol prim)
  (cases primitive prim
    (add-prim () '+)
    (diff-prim () '-)
    (mult-prim () '*)
    (minus-prim () '-)
    (quotient-prim () 'quotient)
    (remainder-prim () 'remainder)
    (zero?-prim () 'zero?)
    (equal?-prim () 'equal?)
    (greater?-prim () '>)
    (less?-prim () '<)
    (cons-prim () 'cons)
    (car-prim () 'car)
    (cdr-prim () 'cdr)
    (null?-prim () 'null?)
    (list-prim () 'list)))

(define (value-of-program/k pgm)
  (cases cps-program pgm
    (cps-a-program (exp) (value-of/k exp (init-env)))))

(define (value-of/k exp env)
  (define (s>> sexps)
    (map (lambda (sexp) (value-of-simple-exp sexp env)) sexps))
  (cases tf-exp exp
    (simple-exp->exp (sexp) (value-of-simple-exp sexp env))
    (cps-let-exp (vars vals body)
      (let ((new-env (extend-env
                       env
                       vars
                       (s>> vals))))
        (value-of/k body new-env)))
    (cps-letrec-exp (p-names b-varss p-bodies body)
      (let ((new-env (extend-env-rec/k env p-names b-varss p-bodies)))
        (value-of/k body new-env)))
    (cps-if-exp (simple1 body1 body2)
      (if (expval->bool (value-of-simple-exp simple1 env))
          (value-of/k body1 env)
          (value-of/k body2 env)))
    (cps-call-exp (rator rands)
      (apply-proc/k
        (expval->cps-proc (value-of-simple-exp rator env))
        (s>> rands)))))

(define-datatype cps-proc cps-proc?
  (a-cps-proc
    (vars (list-of symbol?))
    (body tf-exp?)
    (env environment?)))

(define (extend-env-rec/k env p-names b-varss p-bodies)
  ; assert (= (length p-names) (length b-varss) (length p-bodies))
  (let* ((frm (empty-frame))
         (new-env (extend-env-frame env frm))
         (mk-proc (lambda (vars body)
                    (cps-proc-val (a-cps-proc vars body new-env)))))
    (frame-set! frm p-names (map mk-proc b-varss p-bodies))
    new-env))

(define (apply-proc/k proc vals)
  (cases cps-proc proc
    (a-cps-proc (vars body env)
      (let ((nvars (length vars))
            (nvals (length vals)))
        (if (= nvars nvals)
            (value-of/k body (extend-env env vars vals))
            (report-arguments-not-match vars vals))))))

(define (value-of-simple-exp sexp env)
  (define (s>> sexps)
    (map (lambda (sexp) (value-of-simple-exp sexp env)) sexps))
  (cases simple-exp sexp
    (cps-end-cont ()
      (cps-proc-val
        (a-cps-proc (list 'x) (simple-exp->exp (cps-var-exp 'x)) env)))
    (cps-number-exp (number) (num-val number))
    (cps-var-exp (var) (apply-env env var))
    (cps-apply-primitive-exp (prim rands)
      (apply-primitive prim (s>> rands)))
    (cps-proc-exp (vars body)
      (cps-proc-val
        (a-cps-proc vars body env)))))

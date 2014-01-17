#lang eopl

(#%provide interp
           type-infer
           type->string
           type-poly->string)
(define (interp src) (value-of-program (scan&parse src)))
(define (type-infer src) (type-of-program (scan&parse src)))

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
;
; ExpVal: Number, Boolean, Proc
; DenVal = ExpVal
;
; primitive-procedures: minus, diff(-), addition(+), ,multiplication(*),
;                       quotient, remainder,
;                       zero?, equal?, greater?, less?

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
    (primitive ("less?") less?-prim)))

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

(define (expval->proc val)
  (cases expval val
    (proc-val (proc) proc)
    (else (report-expval-extractor-error 'proc val))))

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

(define (init-env) (empty-env))

(define (apply-primitive prim expvals)
  (define (>> prim/expval) (apply prim/expval expvals))
  (cases primitive prim
    (add-prim () (>> add/expval))
    (diff-prim () (>> diff/expval))
    (mult-prim () (>> mult/expval))
    (minus-prim () (>> minus/expval))
    (quotient-prim () (>> quotient/expval))
    (remainder-prim () (>> remainder/expval))
    (zero?-prim () (>> zero?/expval))
    (equal?-prim () (>> equal?/expval))
    (greater?-prim () (>> greater?/expval))
    (less?-prim () (>> less?/expval))))

; primitive procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add/expval . vals) (num-val (apply + (map expval->num vals))))

(define (diff/expval val1 val2)
  (num-val (- (expval->num val1) (expval->num val2))))

(define (mult/expval . vals) (num-val (apply * (map expval->num vals))))

(define (minus/expval val) (num-val (- (expval->num val))))

(define (quotient/expval val1 val2)
  (num-val (quotient (expval->num val1) (expval->num val2))))

(define (remainder/expval val1 val2)
  (num-val (remainder (expval->num val1) (expval->num val2))))

(define (zero?/expval val) (bool-val (zero? (expval->num val))))

(define (equal?/expval . vals) (bool-val (apply = (map expval->num vals))))

(define (greater?/expval . vals) (bool-val (apply > (map expval->num vals))))

(define (less?/expval . vals) (bool-val (apply < (map expval->num vals))))

; read-eval-print ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-eval-print
  (sllgen:make-rep-loop
    "> " value-of-program
    (sllgen:make-stream-parser
      scanner-spec grammar-spec)))

;(read-eval-print)

; type inference ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-datatype type type?
  (num-type)
  (bool-type)
  (proc-type
    (arg-types (list-of type?))
    (ret-type type?))
  (var-type
    (n integer?)))

(define (proc-type? t)
  (cases type t (proc-type (ats rt) #t) (else #f)))

(define (proc-type-ats t)
  (cases type t (proc-type (ats rt) ats) (else (list 'error t))))

(define (proc-type-rt t)
  (cases type t (proc-type (ats rt) rt) (else (list 'error t))))

(define (var-type? t)
  (cases type t (var-type (n) #t) (else #f)))

(define (join c ls)
  (cond
   ((null? ls) "")
   ((null? (cdr ls)) (car ls))
   (else (string-append (car ls) c (join c (cdr ls))))))

(define (type->string t)
  (cases type t
    (num-type () "num")
    (bool-type () "bool")
    (proc-type (ats rt)
      (let ((ats-str
             (if (null? ats)
                 "()"
                 (join " * " (map type->string ats))))
            (rt-str (type->string rt)))
        (string-append "(" ats-str " -> " rt-str ")")))
    (var-type (n) (string-append "<" (number->string n) ">"))))

(define (type-poly->string t)
  (define (mk-new-symbol)
    (let ((n (- (char->integer #\a) 1)))
      (lambda ()
        (set! n (+ n 1))
        (string #\< (integer->char n) #\>))))
  (let ((new-symbol (mk-new-symbol))
        (symbols '()))
    (define (search-or-new-symbol n ls)
      (if (null? ls)
          (let ((s (new-symbol)))
            (set! symbols (cons (cons n s) symbols))
            s)
          (let ((head (car ls))
                (tail (cdr ls)))
            (if (= n (car head))
                (cdr head)
                (search-or-new-symbol n tail)))))
    (define (>> t)
      (cases type t
        (num-type () "num")
        (bool-type () "bool")
        (proc-type (ats rt)
          (let ((ats-str
                 (if (null? ats)
                     "()"
                     (join " * " (map >> ats))))
                (rt-str (>> rt)))
            (string-append "(" ats-str " -> " rt-str ")")))
        (var-type (n) (search-or-new-symbol n symbols))))
    (>> t)))

(define (type-equal? t1 t2)
  (define (all-equal? ts1 ts2)
    (cond
     ((and (null? ts1) (null? ts2)) #t)
     ((not (or (null? ts1) (null? ts2)))
      (if (type-equal? (car ts1) (car ts2))
          (all-equal? (cdr ts1) (cdr ts2))
          #f))
     (else #f)))
  (cases type t1
    (num-type ()
      (cases type t2
        (num-type () #t)
        (else #f)))
    (bool-type ()
      (cases type t2
        (bool-type () #t)
        (else #f)))
    (proc-type (ats1 rt1)
      (cases type t2
        (proc-type (ats2 rt2)
          (and (type-equal? rt1 rt2)
               (all-equal? ats1 ats2)))
        (else #f)))
    (var-type (n1)
      (cases type t2
        (var-type (n2) (= n1 n2))
        (else #f)))))

(define (constraint left right) (cons left right))
(define (constraint-left c) (car c))
(define (constraint-right c) (cdr c))

(define (empty-constraints) '())

(define (add-constraint C t1 t2)
  (if (type-equal? t1 t2)
      C
      (cons (constraint t1 t2) C)))

(define (constraints-map proc C)
  (if (null? C)
      C
      (let* ((c (car C))
             (p (proc c)))
        (add-constraint (constraints-map proc (cdr C))
                        (constraint-left p)
                        (constraint-right p)))))

(define (add-constraints C ts1 ts2)
  (if (null? ts1)
      C
      (add-constraints
       (add-constraint C (car ts1) (car ts2))
       (cdr ts1)
       (cdr ts2))))

(define (print-constraints C)
  (define (C->strings C)
    (if (null? C)
        '()
        (let ((head (car C)))
          (cons
           (string-append (type->string (constraint-left head))
                          " = "
                          (type->string (constraint-right head)))
           (C->strings (cdr C))))))
  (for-each
   (lambda (s) (display "  ") (display s) (newline))
   (C->strings C)))

(define fresh-type-index -1)

(define (init-fresh-type-index!)
  (set! fresh-type-index -1))

(define (fresh-type)
  (set! fresh-type-index (+ fresh-type-index 1))
  (var-type fresh-type-index))

(define apply-tenv apply-env)

; C is constraints

(define (type-of-program pgm)
  (init-fresh-type-index!)
  (cases program pgm
    (a-program (exp)
      (type-of exp (empty-env) (end-tcont) '()))))

(define (type-of exp tenv tcont C)
  (define (fresh-types n)
    (if (= n 0)
        '()
        (cons (fresh-type) (fresh-types (- n 1)))))
  (cases expression exp
    (number-exp (num) (apply-tcont tcont (num-type) C))
    (var-exp (var) (apply-tcont tcont (apply-tenv tenv var) C))
    (proc-exp (vars body)
      (let ((tmp-types (fresh-types (length vars))))
        (type-of body
                 (extend-env tenv vars tmp-types)
                 (proc-tcont tcont tmp-types)
                 C)))
    (apply-primitive-exp (prim exps)
      (type-of-exps exps
                    tenv
                    (prim-tcont tcont prim)
                    C))
    (call-exp (rator rands)
      (type-of-exps rands
                    tenv
                    (call-tcont tcont tenv rator (fresh-type))
                    C))
    (if-exp (exp1 exp2 exp3)
      (type-of exp1
               tenv
               (if-tcont tcont tenv exp2 exp3)
               C))
    (let-exp (vars exps body)
      (type-of-exps exps
                    tenv
                    (let-tcont tcont tenv vars body)
                    C))
    (letrec-exp (vars p-varss p-bodies body)
      (let* ((tmp-types (fresh-types (length vars)))
             (new-tenv (extend-env tenv vars tmp-types)))
        (type-of-exps (map proc-exp p-varss p-bodies)
                      new-tenv
                      (letrec-tcont tcont new-tenv tmp-types body)
                      C)))))

(define (type-of-exps exps tenv tcont C)
  (if (null? exps)
      (apply-tcont tcont '() C)
      (let ((exp (car exps))
            (rest (cdr exps)))
        (type-of exp tenv (exps-tcont tcont tenv rest) C))))

(define (apply-tcont tcont type C)
  (tcont type C))

(define (exps-tcont tcont tenv exps)
  (lambda (type C)
    (type-of-exps exps tenv (acc-tcont tcont type) C)))

(define (acc-tcont tcont type)
  (lambda (acc C)
    (tcont (cons type acc) C)))

(define (proc-tcont tcont tmp-types)
  (lambda (result-type C)
    (tcont (proc-type tmp-types result-type) C)))

(define (prim-tcont tcont prim)
  (lambda (types C)
    (define (app result-type arg-types)
      (tcont result-type (add-constraints C types arg-types)))
    (define (num-types-n n)
      (if (= n 0)
          '()
          (cons (num-type) (num-types-n (- n 1)))))
    (cases primitive prim
      (add-prim () (app (num-type) (num-types-n (length types))))
      (diff-prim () (app (num-type) (num-types-n 2)))
      (mult-prim () (app (num-type) (num-types-n (length types))))
      (minus-prim () (app (num-type) (num-types-n 1)))
      (quotient-prim ()  (app (num-type) (num-types-n 2)))
      (remainder-prim () (app (num-type) (num-types-n 2)))
      (zero?-prim () (app (bool-type) (num-types-n 1)))
      (equal?-prim () (app (bool-type) (num-types-n 2)))
      (greater?-prim () (app (bool-type) (num-types-n 2)))
      (less?-prim () (app (bool-type) (num-types-n 2))))))

(define (call-tcont tcont tenv rator result-type)
  (lambda (rand-types C)
    (type-of rator tenv (rator-tcont tcont rand-types result-type) C)))

(define (rator-tcont tcont rand-types result-type)
  (lambda (p-type C)
    (tcont result-type
           (add-constraint C
                           p-type
                           (proc-type rand-types result-type)))))

(define (if-tcont tcont tenv exp2 exp3)
  (lambda (type C)
    (type-of-exps (list exp2 exp3)
                  tenv
                  (then-tcont tcont)
                  (add-constraint C type (bool-type)))))

(define (then-tcont tcont)
  (lambda (types C)
    (let ((then-type (car types))
          (else-type (cadr types)))
      (tcont then-type
             (add-constraint C then-type else-type)))))

(define (let-tcont tcont tenv vars body)
  (lambda (types C)
    (type-of body
             (extend-env tenv vars types)
             tcont
             C)))

(define (letrec-tcont tcont tenv tmp-types body)
  (lambda (types C)
    (type-of body
             tenv
             tcont
             (add-constraints C tmp-types types))))

(define (end-tcont)
  (define (substitute old new t)
    (define (s tp) (substitute old new tp))
    (cases type t
      (proc-type (ats rt)
        (proc-type (map s ats) (s rt)))
      (var-type (n)
        (if (type-equal? old t) new t))
      (else t)))
  (define (unify C)
    (if (null? C)
        '()
        (let* ((c (car C))
               (rest (cdr C))
               (l (constraint-left c))
               (r (constraint-right c)))
          (cond
           ((var-type? l)
            (cons (cons l r) (substitute&unify l r rest)))
           ((var-type? r)
            (cons (cons r l) (substitute&unify r l rest)))
           ((and (proc-type? l) (proc-type? r))
            (unify (add-constraints
                    rest
                    (cons (proc-type-rt l) (proc-type-ats l))
                    (cons (proc-type-rt r) (proc-type-ats r)))))
           (else (list 'error l r))))))
  (define (substitute&unify old new C)
    (define (s t) (substitute old new t))
    (unify (constraints-map
            (lambda (c) (cons (s (constraint-left c))
                              (s (constraint-right c))))
            C)))
  (define (solve t U)
    (cases type t
      (var-type (n) (search&substitute t U))
      (proc-type (ats rt)
        (proc-type (map (lambda (tt) (solve tt U)) ats)
                   (solve rt U)))
      (else t)))
  (define (search&substitute t U)
    (if (null? U)
        t
        (let* ((c (car U))
               (rest (cdr U))
               (vt (constraint-left c))
               (val (constraint-right c)))
          (if (type-equal? t vt)
              (solve val rest)
              (search&substitute t rest)))))
  (lambda (t C)
    (let ((U (unify C)))
;      (print-constraints U)
      (solve t U))))

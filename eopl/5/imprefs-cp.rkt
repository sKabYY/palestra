#lang eopl

(#%provide interp)
(define (interp src) (value-of-program (scan&parse src)))
;(define (interp src) (value-of-program-dbg-store (scan&parse src)))

; Program ::= Expression
;
; Expression ::= Number
;            ::= Identifier
;            ::= proc ({Identifier}*(,)) Expression
;            ::= primitive-procedure({Expression}*(,))
;            ::= (Expression {Expression}*)
;            ::= if Expression then Expression else Expression
;            ::= let {Identifier = Expression}*(,) in Expression
;            ::= letrec {Identifier ({Identifier}*(,)) = Expression}*(,) in Expression
;            ::= set Identifier = Expression
;            ::= begin {Expression}*(,) end
;            ::= ref Identifier
;
; MutPair = Ref(ExpVal) X Ref(ExpVal)
; ExpVal = Number + Boolean + Proc + MutPair + Ref(ExpVal)
; DenVal = Ref(ExpVal)
;
; primitive-procedures: minus, diff(-), addition(+), ,multiplication(*),
;                       quotient, remainder,
;                       zero?, equal?, greater?, less?,
;                       newpair, left, right, setleft, setright,
;                       deref, setref,
;
; environment: symbol -> DenVal
; store: Ref -> ExpVal

(define scanner-spec
  '((white-sp (whitespace) skip)
    (commont ("#" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (identifier
      ((or letter)
       (arbno
         (or "-" "_" letter digit))) symbol)))

(define grammar-spec
  '((program (expression) a-program)
    (expression (number) number-exp)
    (expression (identifier) var-exp)
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
      ("set" identifier "=" expression)
      assign-exp)
    (expression
      ("begin" (separated-list expression ",") "end")
      begin-exp)
    (expression
      ("ref" identifier)
      ref-exp)
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
    (primitive ("pair") pair-prim)
    (primitive ("left") left-prim)
    (primitive ("right") right-prim)
    (primitive ("setleft") setleft-prim)
    (primitive ("setright") setright-prim)
    (primitive ("deref") deref-prim)
    (primitive ("setref") setref-prim)))

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
  (proc-val
    (proc proc?))
  (mutpair-val
    (pair mutpair?))
  (ref-val
    (ref reference?)))

(define (ref-val? val)
  (cases expval val
    (ref-val (ref) #t)
    (else #f)))

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

(define (expval->mutpair val)
  (cases expval val
    (mutpair-val (pair) pair)
    (else (report-expval-extractor-error 'mutpair val))))

(define (expval->ref val)
  (cases expval val
    (ref-val (ref) ref)
    (else (report-expval-extractor-error 'ref val))))

(define (minus val)
  (cases expval val
    (num-val (num) (num-val (- num)))
    (else report-expval-extractor-error 'num val)))

; procedure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype proc0 proc?
  (procedure
    (list-of-var (list-of symbol?))
    (body expression?)
    (env environment?)))

(define (proc-val-procedure args body env)
  (proc-val (procedure args body env)))

(define (apply-proc/k proc list-of-val cont)
  (cases proc0 proc
    (procedure (list-of-var body env)
      (let ((nvars (length list-of-var))
            (nvals (length list-of-val)))
        (if (= nvars nvals)
          (value-of/k
            body
            (extend-env env list-of-var list-of-val)
            cont)
          (report-arguments-not-match
            list-of-var list-of-val))))))

(define (report-arguments-not-match vars vals)
  (eopl:error "args not match" vars vals))

; mutpair ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype mutpair mutpair?
  (a-pair
    (left-ref reference?)
    (right-ref reference?)))

(define (newpair val1 val2)
  (a-pair (newref val1) (newref val2)))

(define (mutpair-left pair)
  (cases mutpair pair
    (a-pair (l r) (deref l))))

(define (mutpair-right pair)
  (cases mutpair pair
    (a-pair (l r) (deref r))))

(define (mutpair-setleft pair val)
  (cases mutpair pair
    (a-pair (l r)
      (setref! l val))))

(define (mutpair-setright pair val)
  (cases mutpair pair
    (a-pair (l r)
      (setref! r val))))

; store ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define the-store 'uninitialized)

(define (empty-store) '())

(define (get-store) the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v) (integer? v))

(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val)))
    next-ref))

(define (deref ref)
  (list-ref the-store ref))

(define (setref! ref val)
  (define (iter st ref)
    (if (null? st)
      (report-invalid-reference ref the-store)
      (if (zero? ref)
        (cons val (cdr st))
        (cons (car st) (iter (cdr st) (- ref 1))))))
  (set! the-store
    (iter the-store ref)))

(define (report-invalid-reference ref the-store)
  (eopl:error "invalid reference:" ref the-store))

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
  (extend-env-frame env (a-frame vars (map newref vals))))

(define (extend-env-rec env list-of-name list-of-args list-of-body)
  ; assert (= (length list-of-name) (length list-of-args) (length list-of-body))
  (let* ((frm (empty-frame))
         (new-env (extend-env-frame env frm))
         (mk-proc-ref (lambda (args body)
                        (newref (proc-val-procedure args body new-env)))))
    (frame-set! frm
                list-of-name
                (map mk-proc-ref list-of-args list-of-body))
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

; continuation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype continuation continuation?
  (end-cont)
  (prim-cont
    (saved-cont continuation?)
    (env environment?)
    (prim primitive?)
    (rands (list-of expression?)))
  (prim-cont1
    (saved-cont continuation?)
    (env environment?)
    (prim primitive?)
    (vals (list-of expval?))
    (rands (list-of expression?)))
  (operator-cont
    (saved-cont continuation?)
    (env environment?)
    (rands (list-of expression?)))
  (operands-cont1
    (saved-cont continuation?)
    (env environment?)
    (rator expval?)
    (vals (list-of expval?))
    (rands (list-of expression?)))
  (if-cont
    (saved-cont continuation?)
    (env environment?)
    (then-exp expression?)
    (else-exp expression?))
  (let-cont
    (saved-cont continuation?)
    (env environment?)
    (vars (list-of symbol?))
    (rest-exps (list-of expression?))
    (body expression?))
  (let-cont1
    (saved-cont continuation?)
    (env environment?)
    (vars (list-of symbol?))
    (vals (list-of expval?))
    (exps (list-of expression?))
    (body expression?))
  (assign-cont
    (saved-cont continuation?)
    (env environment?)
    (var symbol?))
  (begin-cont
    (saved-cont continuation?)
    (env environment?)
    (exps (list-of expression?))))

; apply-cont: continuation X expval -> expval
(define (apply-cont cont val)
  (cases continuation cont
    (end-cont () (display "Fin.\n") val)
    (prim-cont (saved-cont env prim rands)
      (apply-cont (prim-cont1 saved-cont env prim '() rands) val))
    (prim-cont1 (saved-cont env prim vals rands)
      (let ((new-vals (cons val vals)))
        (if (null? rands)
          (apply-cont saved-cont (apply-primitive prim (reverse new-vals)))
          (value-of/k (car rands)
                      env
                      (prim-cont1
                        saved-cont env prim new-vals (cdr rands))))))
    (operator-cont (saved-cont env rands)
      (if (null? rands)
        (apply-proc/k (expval->proc val) '() saved-cont)
        (value-of/k (car rands)
                    env
                    (operands-cont1 saved-cont env val '() (cdr rands)))))
    (operands-cont1 (saved-cont env rator vals rands)
      (let ((new-vals (cons val vals)))
        (if (null? rands)
          (apply-proc/k (expval->proc rator) (reverse new-vals) saved-cont)
          (value-of/k (car rands)
                      env
                      (operands-cont1 saved-cont
                                      env rator new-vals (cdr rands))))))
    (if-cont (saved-cont env then-exp else-exp)
      (if (expval->bool val)
        (value-of/k then-exp env saved-cont)
        (value-of/k else-exp env saved-cont)))
    (let-cont (saved-cont env vars rest-exps body)
      (apply-cont (let-cont1 saved-cont env vars '() rest-exps body)
                  val))
    (let-cont1 (saved-cont env vars vals exps body)
      (let ((new-vals (cons val vals)))
        (if (null? exps)
          (value-of/k body
                      (extend-env env vars (reverse new-vals))
                      saved-cont)
          (value-of/k (car exps)
                      env
                      (let-cont1 saved-cont env vars new-vals (cdr exps) body)))))
    (assign-cont (saved-cont env var)
      (setref! (apply-env env var) val)
      (apply-cont saved-cont '**void**))
    (begin-cont (saved-cont env exps)
      (if (null? exps)
        (apply-cont saved-cont val)
        (value-of/k (car exps) env (begin-cont saved-cont env (cdr exps)))))))

; value-of ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (value-of-program pgm)
  (initialize-store!)
  (cases program pgm
    (a-program (exp1)
      (value-of/k exp1 (init-env) (end-cont)))))

(define (print-store)
  (define (iter count lst)
    (if (null? lst)
      'done
      (begin
        (display count)(display " ")
        (eopl:pretty-print (car lst))
        (iter (+ count 1) (cdr lst)))))
  (display "###")(newline)
  (iter 0 the-store)
  (display "###")(newline))

(define (value-of-program-dbg-store pgm)
  (initialize-store!)
  (let ((ret (cases program pgm
               (a-program (exp1)
                 (value-of/k exp1 (init-env) (end-cont))))))
    (print-store)
    ret))

(define (value-of/k exp env cont)
  (cases expression exp
    (number-exp (number)
      (apply-cont cont (num-val number)))
    (var-exp (identifier)
      (apply-cont cont (deref (apply-env env identifier))))
    (proc-exp (list-of-var body)
      (apply-cont cont (proc-val-procedure list-of-var body env)))
    (apply-primitive-exp (prim list-of-exp)
      (if (null? list-of-exp)
        (apply-cont cont (apply-primitive prim '()))
        (value-of/k
          (car list-of-exp)
          env
          (prim-cont cont env prim (cdr list-of-exp)))))
    (call-exp (rator rands)
      (value-of/k rator env (operator-cont cont env rands)))
    (if-exp (exp1 exp2 exp3)
      (value-of/k exp1 env (if-cont cont env exp2 exp3)))
    (let-exp (list-of-symbol list-of-exp body)
      (if (null? list-of-symbol)
        (value-of/k body env cont)
        (value-of/k
          (car list-of-exp)
          env
          (let-cont cont env list-of-symbol (cdr list-of-exp) body))))
    (letrec-exp (list-of-name list-of-args list-of-body letrec-body)
      (let ((new-env (extend-env-rec
                       env
                       list-of-name
                       list-of-args
                       list-of-body)))
        (value-of/k letrec-body new-env cont)))
    (assign-exp (var exp)
      (value-of/k exp env (assign-cont cont env var)))
    (begin-exp (list-of-exp)
      (if (null? list-of-exp)
        (report-no-exps-in-begin)
        (value-of/k (car list-of-exp)
                    env
                    (begin-cont cont env (cdr list-of-exp)))))
    (ref-exp (var)
      (apply-cont cont (ref-val (apply-env env var))))))

(define (report-no-exps-in-begin)
  (eopl:error "no expressions between begin and end"))

(define (init-env) (empty-env))

(define (apply-primitive prim list-of-expval)
  (cases primitive prim
    (add-prim ()
      (num-val (apply + (map expval->num list-of-expval))))
    (diff-prim ()
      (num-val (apply - (map expval->num list-of-expval))))
    (mult-prim ()
      (num-val (apply * (map expval->num list-of-expval))))
    (minus-prim () (apply minus list-of-expval))
    (quotient-prim ()
      (num-val (apply quotient (map expval->num list-of-expval))))
    (remainder-prim ()
      (num-val (apply remainder (map expval->num list-of-expval))))
    (zero?-prim ()
      (bool-val (apply zero? (map expval->num list-of-expval))))
    (equal?-prim ()
      (bool-val (apply = (map expval->num list-of-expval))))
    (greater?-prim ()
      (bool-val (apply > (map expval->num list-of-expval))))
    (less?-prim ()
      (bool-val (apply < (map expval->num list-of-expval))))
    (pair-prim ()
      (mutpair-val (apply newpair list-of-expval)))
    (left-prim ()
      (apply mutpair-left (map expval->mutpair list-of-expval)))
    (right-prim ()
      (apply mutpair-right (map expval->mutpair list-of-expval)))
    (setleft-prim () (apply setleft-wrapper list-of-expval))
    (setright-prim () (apply setright-wrapper list-of-expval))
    (deref-prim () (apply deref (map expval->ref list-of-expval)))
    (setref-prim () (apply setref-wrapper list-of-expval))))


(define (setleft-wrapper expv1 expv2)
  (mutpair-setleft (expval->mutpair expv1) expv2)
  '**void**)

(define (setright-wrapper expv1 expv2)
  (mutpair-setright (expval->mutpair expv1) expv2)
  '**void**)

(define (setref-wrapper expv1 expv2)
  (setref! (expval->ref expv1) expv2)
  '**void**)

; read-eval-print ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-eval-print
  (sllgen:make-rep-loop
    "> " value-of-program
    (sllgen:make-stream-parser
      scanner-spec grammar-spec)))

;(read-eval-print)

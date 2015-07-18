; lisp

(require scheme/mpair)

(define (m-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
        ((application? exp)
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (m-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (immutable->mutable (procedure-parameters procedure))
             (immutable->mutable arguments)
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (m-eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (m-eval (if-predicate exp) env))
    (m-eval (if-consequent exp) env)
    (m-eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (m-eval (first-exp exps) env))
        (else (m-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (m-eval (definition-value exp) env)
                    env)
  'ok)

; self-evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

; variable
(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

; quote
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

; assignment
(define (assignment? exp)
  (tagged-list? exp 'set))
(define (assignment-variable? exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; 1. define symbol:
;   (define <var> <value>)
; 2. define proc:
;   (define (<var> <p1> <p2> ...) <body>)

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)  ; define symbol
    (caadr exp)))  ; define proc
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)  ; define symbol
    (make-lambda (cdadr exp) (cddr exp))))  ; define proc

; lambda
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; if
(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin
(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; cond
(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car exp))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false  ; clause else no
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF" clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))

(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (mlength vars) (mlength vals))
    (cons (make-frame vars vals) base-env)
    (if (< (mlength vars) (mlength vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (scan-a-frame
          var
          found-action
          not-found-action
          frame)
  (define (scan vars vals)
    (cond ((null? vars)
           (not-found-action))
          ((eq? var (mcar vars))
           (found-action vals))
          (else
            (scan (mcdr vars) (mcdr vals)))))
  (scan (frame-variables frame) (frame-values frame)))

(define (lookup-and-dosth-variable
          var
          found-action
          not-found-action
          env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
      (not-found-action)
      (scan-a-frame
        var
        found-action
        (lambda () (env-loop (enclosing-environment env)))
        (first-frame env))))
  (env-loop env))

(define (lookup-variable-value var env)
  (lookup-and-dosth-variable
    var
    (lambda (vals) (mcar vals))
    (lambda () (error "Unbound variable" var))
    env))

(define (set-variable-value! var val env)
  (lookup-and-dosth-variable
    var
    (lambda (vals) (set-mcar! vals val))
    (lambda () (error "Unbound variable -- SET!" var))
    env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan-a-frame
      var
      (lambda (vals) (set-mcar! vals val))
      (lambda () (add-binding-to-frame! var val frame))
      frame)))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (cons 'car car)
        (cons 'cdr cdr)
        (cons 'cons cons)
        (cons 'null? null?)
        (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '/ /)
        (cons 'quotient quotient)
        (cons 'remainder remainder)
        (cons '= =)
        ; TODO
        ))

(define (immutable->mutable l)
  (if (null? l)
    '()
    (mcons (car l) (immutable->mutable (cdr l)))))

(define (primitive-procedure-names)
  (immutable->mutable (map car primitive-procedures)))
(define (primitive-procedure-objects)
  (immutable->mutable (map
                        (lambda (proc) (list 'primitive (cdr proc)))
                        primitive-procedures)))

(define (apply-primitive-procedure proc args)
  (define apply-in-underlying-scheme apply)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define the-global-environment (setup-environment))

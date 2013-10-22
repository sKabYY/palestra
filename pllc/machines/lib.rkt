; continuation
(define mt '())
(define mt? null?)

; environment
(define (mkenv) '())
(define empty-env? null?)

(define (ext-env env x c) (cons (cons x c) env))

(define (lookup-env env x)
  (if (empty-env? env)
    (error "Unbound symbol -- LOOKUP-ENV" x)
    (if (same-variable? (caar env) x)
      (cdar env)
      (lookup-env (cdr env) x))))

; expression
(define (lambda? exp) (and (pair? exp) (eq? (car exp) 'lambda)))
(define (variable? exp) (symbol? exp))
(define (constant? exp) (number? exp))
(define (not-variable-value? v) (or (lambda? v) (constant? v)))
(define (value? v) (or (not-variable-value? v) (variable? v)))

; assert (variable? a) and (variable? b)
(define (same-variable? a b) (eq? a b))

; primitive operations
(define primitive-operations
  (list
    (cons '+ +)
    (cons '- -)
    (cons '* *)
    (cons '/ quotient)
    (cons '% remainder)
    ))

(define (search-operation op)
  (define (iter ops)
    (cond
      ((null? ops) #f)
      ((eq? op (caar ops)) (cdar ops))
      (else (iter (cdr ops)))))
  (iter primitive-operations))

(define (primitive-operation? f) (search-operation f))

(define (primitive-apply op as)
  (apply (search-operation op) as))

; reduction
(define (beta-reduce M x v)
  (match M
         ((? variable? x0) (if (same-variable? x0 x) v x0))
         ((? constant? b) b)
         (`(lambda (,x0) ,body)
          (if (same-variable? x0 x)
            M
            `(lambda (,x0) ,(beta-reduce body x v))))
         ((? primitive-operation? o) o)
         ((? pair? application)
          (map (lambda (m) (beta-reduce m x v)) application))
         (else
           (error "Unknown M type -- BETA-REDUCE" M))))

; driver-loop
(define (driver-loop eval)
  (let ((input (read-input)))
    (if (eof-object? input)
      (begin (newline)(display 'Bye~)(newline))
      (let ((output (with-handlers
                      ((exn:fail?
                         (lambda (x)
                           (string-append "Error: " (exn-message x)))))
                      (eval input))))
        (display-output output)
        (driver-loop eval)))))

(define input-prompt "> ")
(define output-prompt "")

(define (read-input)
  (display input-prompt)(read))

(define (display-output output)
  (display output-prompt)(display output)(newline))

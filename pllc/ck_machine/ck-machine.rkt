;

(define (new-printer)
  (define step_num 1)
  (lambda (ec-pair)
    (display "\t")(display step_num)(display ": ")
    (set! step_num (+ step_num 1))
    (display '<)(display (car ec-pair))(display '>)
    (display "  ")
    (display '<)(display (cdr ec-pair))(display '>)
    (newline)))

(define (eval-ck expression)
  (define printer (new-printer))
  (define (iter ec-pair)
    ; Show the reduction
    (printer ec-pair)
    ;;;;;;;;;;;;;;;;;;;;
    (let ((expr (car ec-pair))
          (cont (cdr ec-pair)))
      (if (and (value? expr) (mt? cont))
        expr
        (iter (reduce-ck1 ec-pair)))))
  (iter (cons expression mt)))

(define mt '())
(define mt? null?)
(define primitive-operations (list
              (cons '+ +)
              ))

; ec-pair: expression-continuation pair
(define (reduce-ck1 ec-pair)
  (let ((expression (car ec-pair))
        (continuation (cdr ec-pair)))
    (if (value? expression)
      (match continuation
             ; fun
             (`(fun (lambda (,x) ,M) ,k)
              (cons (beta-reduce M x expression) k))
             ; arg
             (`(arg ,N ,k)
              (cons N (list 'fun expression k)))
             ; opd
             (`(opd ,values-op ,rest ,k)
              (if (null? rest)
                (cons
                  (reverse-primitive-apply (cons expression values-op))
                  k)
                (cons
                  (car rest)
                  (list 'opd (cons expression values-op) (cdr rest) k))))
             ; else error
             (else
               (error "Unknown continuation -- REDUCE-CK1" continuation)))
      (let ((f (car expression))
            (as (cdr expression)))
        (if (primitive-operation? f)
          (cons (car as) (list 'opd (list f) (cdr as) continuation))
          (if (null? (cdr as))
            (cons f (list 'arg (car as) continuation))
            (error "application should have just one argument -- REDUCE-CK1" expression)))))))

(define (lambda? exp) (and (pair? exp) (eq? (car exp) 'lambda)))
(define (variable? exp) (symbol? exp))
(define (constant? exp) (number? exp))
(define (value? v) (or (lambda? v) (constant? v) (variable? v)))

; assert (variable? a) and (variable? b)
(define (same-variable? a b) (eq? a b))

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

(define (search-operation op)
  (define (iter ops)
    (cond
      ((null? ops) #f)
      ((eq? op (caar ops)) (cdar ops))
      (else (iter (cdr ops)))))
  (iter primitive-operations))

(define (primitive-operation? f) (search-operation f))

(define (reverse-primitive-apply l)
  (define (iter op as)
    (if (null? (cdr op))
      (primitive-apply (car op) as)
      (iter (cdr op) (cons (car op) as))))
  (iter l '()))

(define (primitive-apply op as)
  (apply (search-operation op) as))

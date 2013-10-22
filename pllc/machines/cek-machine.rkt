(load "lib.rkt")

(define MAX-STEPS 50)

; printer

(define (display-environment env)
  (if (empty-env? env)
    (void)
    (let ((x (caar env))
          (c (cdar env)))
      (display #\[)(display x)(display #\:)
      (display-closure c)
      (display #\])
      (display-environment (cdr env)))))

(define (display-closure closure)
  (display '<)
  (display (closure-expression closure))
  (display ", ")
  (display-environment (closure-environment closure))
  (display '>))

(define (new-printer)
  (define step-num 1)
  (lambda (cc-pair)
    (display "\t")(display step-num)(display ": ")
    (display-closure (cc-closure cc-pair))
    (display '<)(display (cc-continuation cc-pair))(display '>)
    (newline)
    (set! step-num (+ step-num 1))
    (if (> step-num MAX-STEPS)
      (error "Reach MAX-STEPS:" MAX-STEPS)
      (void))))

; cc-pair: closure-continuation pair
(define (eval-cek expression)
  (define printer (new-printer))
  (define (iter cc-pair)
    ;;;
    (printer cc-pair)
    ;;;
    (let ((expr (closure-expression (cc-closure cc-pair)))
          (cont (cc-continuation cc-pair)))
      (if (and (not-variable-value? expr) (mt? cont))
        expr
        (iter (reduce-cek cc-pair)))))
  (iter (cons (cons expression (mkenv)) mt)))

(define (make-cc closure continuation)
  (cons closure continuation))
(define (cc-closure cc-pair) (car cc-pair))
(define (cc-continuation cc-pair) (cdr cc-pair))

(define (make-closure expression environment)
  (cons expression environment))
(define (closure-expression closure) (car closure))
(define (closure-environment closure) (cdr closure))

(define (continuation-push . args) args)

(define (reduce-cek cc-pair)
  (define clos (cc-closure cc-pair))
  (let ((expr (closure-expression clos))
        (env (closure-environment clos))
        (cont (cc-continuation cc-pair)))
    (cond
      ((variable? expr) (make-cc (lookup-env env expr) cont))
      ((value? expr)
       (match cont
              (`(fun ((lambda (,x) ,M) . ,e) ,k)
               (make-cc (make-closure M (ext-env e x clos)) k))
              (`(arg (,N . ,e) ,k)
               (make-cc (make-closure N e)
                        (continuation-push 'fun (make-closure expr env) k)))
              (`(opd ,values-op ,rest ,k)
               (if (null? rest)
                 (make-cc
                   (make-closure
                     (reverse-primitive-apply (cons clos values-op))
                     (mkenv))
                   k)
                 (make-cc
                   (car rest)
                   (continuation-push
                     'opd
                     (cons clos values-op)
                     (cdr rest)
                     k))))
              (else
                (error "Unknown continuation -- REDUCE-CEK" cont))))
      (else
        (let ((f (car expr))
              (as (cdr expr)))
          (if (primitive-operation? f)
            (make-cc
              (make-closure (car as) env)
              (continuation-push
                'opd
                (list f)
                (map (lambda (a) (make-closure a env)) (cdr as))
                cont))
            (if (null? (cdr as))
              (make-cc
                (make-closure f env)
                (continuation-push
                  'arg
                  (make-closure (car as) env)
                  cont))
              (error "application should have just one argument -- REDUCE-CEK" expr))))))))

(define (reverse-primitive-apply l)
  (define (iter op as)
    (if (null? (cdr op))
      (primitive-apply (car op) as)
      (iter (cdr op) (cons (closure-expression (car op)) as))))
  (iter l '()))

; Run!
(driver-loop eval-cek)

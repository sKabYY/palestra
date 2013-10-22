(load "lib.rkt")

(define MAX-STEPS 50)

(define (new-printer)
  (define step-num 1)
  (lambda (ec-pair)
    (display "\t")(display step-num)(display ": ")
    (display '<)(display (car ec-pair))(display '>)
    (display "  ")
    (display '<)(display (cdr ec-pair))(display '>)
    (newline)
    (set! step-num (+ step-num 1))
    (if (> step-num MAX-STEPS)
      (error "Reach MAX-STEPS:" MAX-STEPS)
      (void))))

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
        (iter (reduce-ck ec-pair)))))
  (iter (cons expression mt)))

; ec-pair: expression-continuation pair
(define (reduce-ck ec-pair)
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
               (error "Unknown continuation -- REDUCE-CK" continuation)))
      (let ((f (car expression))
            (as (cdr expression)))
        (if (primitive-operation? f)
          (cons (car as) (list 'opd (list f) (cdr as) continuation))
          (if (null? (cdr as))
            (cons f (list 'arg (car as) continuation))
            (error "application should have just one argument -- REDUCE-CK1" expression)))))))

(define (reverse-primitive-apply l)
  (define (iter op as)
    (if (null? (cdr op))
      (primitive-apply (car op) as)
      (iter (cdr op) (cons (car op) as))))
  (iter l '()))

; Run!
(driver-loop eval-ck)

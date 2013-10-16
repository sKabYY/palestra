;

(define (eval-ck expression)
  (eval-ck1 expression mt))

(define mt '())

(define (eval-ck1 expression continuation)

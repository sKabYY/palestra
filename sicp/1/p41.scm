(define (double f) (lambda (x) (f (f x))))
(define (inc n) (+ n 1))
(display (((double (double double)) inc) 5))
(newline)

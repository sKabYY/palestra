(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (multi a b)
  (define (multi-iter m a b)
    (cond ((= b 0) m)
          ((even? b) (multi-iter m (double a) (halve b)))
          (else (multi-iter (+ m a) a (- b 1)))))
  (multi-iter 0 a b))

(define (print n)
  (begin
    (display n)
    (newline)))

(print (multi 3 8))
(print (multi 8 8))
(print (multi 9 7))
(print (multi 99 101))

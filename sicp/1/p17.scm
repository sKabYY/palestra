(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (multi a b)
  (cond ((= b 0) 0)
        ((even? b) (multi (double a) (halve b)))
        (else (+ a (multi a (- b 1))))))

(define (print n)
  (begin
    (display n)
    (newline)))

(print (multi 3 8))
(print (multi 8 8))
(print (multi 9 7))
(print (multi 99 101))

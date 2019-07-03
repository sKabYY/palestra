(load "mk.ss")

(define (from-list lst)
  (if (null? lst) (empty-stream) (make-stream (car lst) (delay (from-list (cdr lst))))))

(define (print-stream n s)
  (if (or (stream-null? s) (and n (= n 0)))
    (printf "~n")
    (begin
      (printf "~a," (stream-car s))
      (print-stream (and n (- n 1)) (stream-cdr s)))))

(let ([i (from-list '(1 2 3 4 5))] [s (from-list '(a b c d e))])
  (print-stream #f i)
  (print-stream #f s)
  (print-stream #f (stream-append i (delay s)))
  )

(define int
  (letrec ([from (lambda (i)
                   (make-stream i (delay (from (+ i 1)))))])
    (from 1)))

(print-stream 5 int)
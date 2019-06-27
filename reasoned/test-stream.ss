(load "mk.ss")

(define (from-list lst)
  (if (null? lst) (empty-stream) (make-stream (car lst) (delay (from-list (cdr lst))))))

(define (print-stream s)
  (if (stream-null? s)
    (printf "~n")
    (begin
      (printf "~a," (stream-fst s))
      (print-stream (stream-rst s)))))

(let ([i (from-list '(1 2 3 4 5))] [s (from-list '(a b c d e))])
  (print-stream i)
  (print-stream s)
  (print-stream (stream-append i (delay s)))
  (print-stream (stream-flatten (from-list (list i s))))
  ;(print-stream (stream-join-map list i (delay s)))
  )
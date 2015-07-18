(load "stream.scm")

; Don't run this file!!!

(define (wrong-pairs s t)
  (interleave
    (stream-map-n (lambda (x) (list (stream-car s) x)) t)
    (wrong-pairs (stream-cdr s) (stream-cdr t))))

;(define hehe (wrong-pairs integers integers))

;(stream-for-n println hehe 10)
(display (stream-car
           (wrong-pairs integers integers)
           ))(newline)

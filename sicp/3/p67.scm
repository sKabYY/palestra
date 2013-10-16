(load "stream.scm")

(define (pairs2 s t)
  (cons
    (list (stream-car s) (stream-car t))
    (delay
      (interleave
        (interleave
          (stream-map-n (lambda (x) (list (stream-car s) x))
                        (stream-cdr t))
          (stream-map-n (lambda (x) (list x (stream-car t)))
                        (stream-cdr s)))
        (pairs2 (stream-cdr s) (stream-cdr t))))))

(stream-for-n
  println
  (pairs2 integers integers)
  10)

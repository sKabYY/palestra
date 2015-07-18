(load "stream.scm")

(define (triples s t u)
  (cons
    (list (stream-car s) (stream-car t) (stream-car u))
    (delay
      (interleave
        (stream-cdr
          (stream-map-n
            (lambda (x) (cons (stream-car s) x))
            (pairs t u)))
        (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define itriples (triples integers integers integers))

(define (pythagoras? x y z)
  (= (+ (square x) (square y)) (square z)))

(define pythagoras-triples
  (stream-filter-0
    (lambda (t) (apply pythagoras? t))
    itriples))

(stream-for-n
  println
  pythagoras-triples
  5)

(load "stream.scm")

(define (show x)
  (begin
    (display x)
    (newline)
    x))

(define x (stream-map-n show integers))

(stream-ref-0 x 5)
(stream-ref-0 x 7)

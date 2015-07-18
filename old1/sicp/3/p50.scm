(load "stream.scm")

(define s
  (stream-map-n + integers (stream-cdr integers)))

(define evens
  (stream-map-n (lambda (x) (* x 2)) integers))

(display (stream-for-n
           (lambda (x)
             (begin
               (display x)
               (newline)))
           evens
           10))
(newline)

(display (stream-for-n
           (lambda (x)
             (begin
               (display x)
               (newline)))
           s
           10))
(newline)

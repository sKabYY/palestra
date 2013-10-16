(load "stream.scm")

(define (pi-summands n)
  (cons
    (/ 1.0 n)
    (delay (stream-map-n - (pi-summands (+ n 2))))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (display-10 s)
  (stream-for-n println s 10))

(println "#1:")
(display-10 pi-stream)

(println "#2:")
(display-10 (euler-transform pi-stream))

(println "#3:")
(display-10 (euler-transform (euler-transform pi-stream)))

(println "#super:")
(display-10 (accelerated-sequence euler-transform pi-stream))

(load "stream.scm")

(define (ln2-summands n)
  (cons (/ 1.0 n)
        (delay (stream-map-n - (ln2-summands (+ n 1))))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define (display-10 s)
  (stream-for-n println s 10))

(println "#1:")
(display-10 ln2-stream)

(println "#2:")
(display-10 (euler-transform ln2-stream))

(define (repeated f n)
  (lambda (x)
    (if (= n 0)
      x
      ((repeated f (- n 1)) (f x)))))

(println "#repeated:")
(display-10 ((repeated euler-transform 7) ln2-stream))

(println "super:")
(display-10 (accelerated-sequence euler-transform ln2-stream))

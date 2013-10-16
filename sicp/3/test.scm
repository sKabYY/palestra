(load "weighted-pairs.scm")

(define (weight i j) (+ i j))

(define ipairs (weighted-pairs
                 integers
                 integers
                 weight))

(stream-for-n
  (lambda (x) (if (> (car x) 9000) (println x) (void)))
  ipairs
  100000)

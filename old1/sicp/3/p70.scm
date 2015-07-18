(load "weighted-pairs.scm")

(define test (weighted-pairs integers integers
                             (lambda (i j) (+ i j))))

(println "test)")
(stream-for-n println test 10)
(newline)

(define a (weighted-sorted-pairs integers integers
                                 (lambda (i j) (+ i j))))
(println "a)")
(stream-for-n println a 10)
(newline)

(define b (weighted-sorted-pairs
            integers integers
            (lambda (i j) (+ (* 2 i) (* 3 j) (* 5 i j)))))

(println "b)")
(stream-for-n println b 10)
(newline)

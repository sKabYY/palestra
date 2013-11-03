(load "weighted-pairs-new.scm")

(define test (weighted-pairs-new integers integers
                             (lambda (i j) (+ i j))))

(println "test)")
(stream-for-n println test 100)
(newline)
(println merge-count)

(load "stream.scm")

(define S (cons
            1
            (delay (merge
                     (merge (scale-stream S 2)
                            (scale-stream S 3))
                     (scale-stream S 5)))))

(stream-for-n
  println
  ;(lambda (x) (if (> x 9000) (println x) (void)))
  S
  20)

(define (fast-expt b n)
  (define (fast-expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter a (* b b) (/ n 2)))
          (else (fast-expt-iter (* a b) b (- n 1)))))
  (fast-expt-iter 1 b n))

(define (print n)
  (begin
    (display n)
    (newline)))

(print (fast-expt 2 10))
(print (fast-expt 3 10))
(print (fast-expt 3 3))

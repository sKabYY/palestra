(load "lib.scm")

(define (unique-3-tuple n)
  (flatmap
    (lambda (k)
      (map (lambda (p)
             (list (car p) (cadr p) k))
           (unique-pairs (- k 1))))
    (enumerate-interval 1 n)))

(define (sum-s n s)
  (define (sum-equal-s? tuple)
    (= s (+ (car tuple) (cadr tuple) (caddr tuple))))
  (filter sum-equal-s?
          (unique-3-tuple n)))

(display (sum-s 6 10))
(newline)


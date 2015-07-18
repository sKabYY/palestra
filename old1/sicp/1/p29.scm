(define (sum1 term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum1 term (next a) next b))))

(define (sum term a next b)
  (define (sum-iter s a)
    (if (> a b)
      s
      (sum-iter (+ s (term a)) (next a))))
  (sum-iter 0 a))

(define (integral f a b n)
  (define (integral-sum-h h)
    (define (term i)
      (cond ((= i 0) (f (+ a (* i h))))
            ((= i n) (f (+ a (* i h))))
            ((even? i) (* 2 (f (+ a (* i h)))))
            (else (* 4 (f (+ a (* i h)))))))
    (* (/ h 3)
       (sum term 0 (lambda (n) (+ n 1)) n)))
  (integral-sum-h (/ (- b a) n)))

(define (print n)
  (begin
    (display n)
    (newline)))

(define (cube x) (* x x x))

(print (integral cube 0 1 100.))
(print (integral cube 0 1 100000.))
(print (integral cube 0 1 101.))

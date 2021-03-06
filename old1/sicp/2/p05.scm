(define (cons0 a b) (* (expt 2 a) (expt 3 b)))

(define (p-log n p)
  (define (test? m)
    (= (remainder n (expt p m)) 0))
  (define (search a b)
    (if (= (+ a 1) b)
      a
      (let ((m (/ (+ a b) 2)))
        (if (test? m)
          (search m b)
          (search a m)))))
  (define (find-up-boundary b)
    (if (not (test? b))
      b
      (find-up-boundary (* b 2))))
  (search 0 (find-up-boundary 1)))

(define (car0 n) (p-log n 2))
(define (cdr0 n) (p-log n 3))

(define n (cons0 3 5))
(display n)
(newline)
(display (car0 n))
(newline)
(display (cdr0 n))
(newline)

(define l (cons0 1 (cons0 2 (cons0 3 0))))
(display l)
(newline)
(display (car0 l))
(newline)
(display (car0 (cdr0 l)))
(newline)
(display (car0 (cdr0 (cdr0 l))))
(newline)

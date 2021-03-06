(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f2 n)
  (define (f2-iter a0 a1 a2 n)
    (if (= n 0)
      a2
      (f2-iter a1 a2 (+ a2 (* 2 a1) (* 3 a0)) (- n 1))))
  (if (< n 3)
    n
    (f2-iter 0 1 2 (- n 2))))

(display (f 1))
(newline)
(display (f 2))
(newline)
(display (f 3))
(newline)
(display (f 4))
(newline)
(display (f 5))
(newline)
(newline)
(display (f2 1))
(newline)
(display (f2 2))
(newline)
(display (f2 3))
(newline)
(display (f2 4))
(newline)
(display (f2 5))
(newline)

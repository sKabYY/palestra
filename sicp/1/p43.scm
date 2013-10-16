(define (repeated f n)
  (lambda (x)
    (if (<= n 0)
      x
      (f ((repeated f (- n 1)) x)))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated2 f n)
  (if (<= n 0)
    (lambda (x) x)
    (compose f (repeated f (- n 1)))))

(define (repeated3 f n)
  (define (iter result f n)
    (if (<= n 0)
      result
      (iter (compose f result) f (- n 1))))
  (iter (lambda (x) x) f n))

(define (square x) (* x x))

(display ((repeated square 2) 5))
(newline)
(display ((repeated2 square 2) 5))
(newline)
(display ((repeated3 square 2) 5))
(newline)

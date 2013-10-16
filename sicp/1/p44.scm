(define (smooth f dx)
  (define (average3 a b c)
    (/ (+ a b c) 3))
  (lambda (x)
    (average3 (f (- x dx)) (f x) (f (+ x dx)))))

(define (smooth-1 f)
  (smooth f 0.0001))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter result f n)
    (if (<= n 0)
      result
      (iter (compose f result) f (- n 1))))
  (iter (lambda (x) x) f n))

(define (smooth-n f n)
  ((repeated smooth-1 n) f))

(define (displayf f a b dx)
  (if (> a b)
    (void)
    (begin
      (display "f(")
      (display a)
      (display ")=")
      (display (f a))
      (newline)
      (displayf f (+ a dx) b dx))))

(define (square x) (* x x))

(displayf (repeated square 10) 0 1 0.1)

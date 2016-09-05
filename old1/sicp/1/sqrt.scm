(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (sqrt x)
  (define (sqrt-iter guess)
    (define (good-enough?)
      (< (abs (- (square guess) x)) 0.000001))
    (define (improve)
      (average guess (/ x guess)))
    (display "*")
    (if (good-enough?)
      guess
      (sqrt-iter (improve))))
  (sqrt-iter 1.0))

(define (print-sqrt-loop n)
  (define (iter i)
    (begin
      (display (sqrt i))
      (newline)
      (if (> i n)
        (void)
        (iter (+ i 1)))))
  (iter 0))
(print-sqrt-loop 10)
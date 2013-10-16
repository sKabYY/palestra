(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product
    (lambda (x) x)
    1
    (lambda (x) (+ x 1))
    n))

(define (print n)
  (begin
    (display n)
    (newline)))

(print (factorial 5))
(print (factorial 4))
(print (factorial 3))

(define (pi n)
  (* 4. (product
         (lambda (x)
           (if (even? x)
             (/ (+ x 2) (+ x 1))
             (/ (+ x 1) (+ x 2))))
         1
         (lambda (x) (+ x 1))
         n)))

(print (pi 10000))

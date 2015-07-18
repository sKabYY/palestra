#lang eopl

(define (fact n)
  (if (zero? n) 1 (* n (fact (- n 1)))))

(define (fact-iter n)
  (define (iter acc n)
    (if (zero? n) acc (iter (* acc n) (- n 1))))
  (iter 1 n))

(define (fact/k n)
  (define (iter n k)
    (if (zero? n)
        (k 1)
        (iter (- n 1) (lambda (x) (k (* n x))))))
  (iter n (lambda (x) x)))

(define (fact/k-2 n)
  (define (iter n k)
    (if (zero? n)
        (apply/k k 1)
        (iter (- n 1) (cons n k))))
  (define (apply/k k n)
    (if (null? k) n (apply/k (cdr k) (* n (car k)))))
  (iter n '()))

(eopl:pretty-print (fact 5))
(eopl:pretty-print (fact-iter 5))
(eopl:pretty-print (fact/k 5))
(eopl:pretty-print (fact/k-2 5))

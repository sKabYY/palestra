#lang racket

(provide (all-defined-out))

(define test-iswim
  '(

1 1
a a
(add1 32) 33
(sub1 33) 32
(iszero 11) #f
(iszero 0) #t
(+ 2 2) 4
(- 1 1) 0
(* 3 4) 12
(iszero (* 3 (- 1 1))) #t

((lambda (x) (+ x 1)) 2) 3

))

(define test-cek1
  '(

((lambda () 1)) 1
((lambda (x y) (+ x y)) 2 3) 5

))

(define test-cek (append test-iswim test-cek1))

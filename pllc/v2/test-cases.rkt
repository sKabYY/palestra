#lang racket

(provide (all-defined-out))

(define test-iswim
  '(

1 1
(add1 32) 33
(sub1 33) 32
(iszero 11) #f
(iszero 0) #t
(+ 2 2) 4
(- 1 1) 0
(* 3 4) 12
(iszero (* 3 (- 1 1))) #t

((lambda (x) (+ x 1)) 2) 3
((lambda (x) (+ x 1)) (+ 1 1)) 3

))

(define test-cek1
  '(

((lambda () 1)) 1
((lambda (x y) (+ x y)) 11 3) 14

))

(define test-cek (append test-iswim test-cek1))

(define test-ceks1
  '(

(let ((a 12) (b 3)) (+ a b)) 15
(let ((x 0)) (begin (set! x 1) x)) 1

(+
 (let ((a 2)
       (b 3)
       (c 4))
   (* a b c))
 (let ((a 1)
       (b 2)
       (c 3))
   (+ a b c))) 30

(let ((x 7))
  (+
   (let ((a 2)
         (b 3)
         (c 4))
     (* a b c))
   (let ((a 1)
         (b 2)
         (c 3))
     (+ a b c))
   x)) 37

))

(define test-ceks (append test-cek test-ceks1))

(define test-cekhs1
  '(

(+ 1 (letcc x (+ (lambda (y) y) (cc x 12)))) 13

((lambda (x) x) (throw 1)) 1

(catch
 (let ((a 1))
   (+ a (throw 12)))
 with x
 (let ((a 2))
   (+ a x))) 14

))

(define test-cekhs (append test-ceks test-cekhs1))

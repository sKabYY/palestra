#lang racket
(require "error.rkt")

(provide (all-defined-out))

(define color-on #t)
(define (color code) (if color-on (display code) (void)))
(define (begin-red) (color "\033[91m"))
(define (begin-green) (color "\033[92m"))
(define (begin-yellow) (color "\033[93m"))
(define (end-color) (color "\033[0m"))

(define (test interp cases)
  (define (iter cases total wrong)
    (if (null? cases)
        (printf "SUMMARY(wrong/total): ~a/~a.~%" wrong total)
        (let ((exp1 (car cases))
              (ans (cadr cases))
              (rest (cddr cases)))
          (begin-green)
          (pretty-print exp1)
          (begin-red)
          (let ((correct (with-handlers
                           [(error:interp?
                             (lambda (e)
                               (displayln (error:interp-message e))
                               (eq? #t (ans e))))]
                           (let ((ret (interp exp1)))
                             (display ">> ")
                             (pretty-print ret)
                             (equal? ret ans)))))
            (begin-yellow)
            (if correct
                (void)
                (printf "[WRONG!]ans=~a~%" ans))
            (end-color)
            (newline)
            (iter rest (+ total 1) (if correct wrong (+ wrong 1)))))))
  (iter cases 0 0))


(define cases
  `(

((main (output 42))) 42
((main (output #t))) #t
((main (output #f))) #f
((main (var i 42) (output i))) 42
((main (var b #t) (output b))) #t

((main (output (array 3 42)))) ,(make-vector 3 42)
((main (output (array 5 #t)))) ,(make-vector 5 #t)
((main (output (array 0 #t)))) ,error:mk-array-invalid-length?
((main (output (array -1 #t)))) ,error:mk-array-invalid-length?

((main (var arr (array 3 42)) (output (array-ref arr 0)))) 42
((main (var arr (array 3 42)) (output (array-ref arr 1)))) 42
((main (var arr (array 3 42)) (output (array-ref arr 2)))) 42
((main (var arr (array 3 42))
       (output (array-ref arr -1)))) ,error:index-out-of-range?
((main (var arr (array 3 42))
       (output (array-ref arr 3)))) ,error:index-out-of-range?

((main (var arr (array 3 42))
       (array-set! arr 0 2)
       (output (array-ref arr 0)))) 2
((main (var arr (array 3 42))
       (array-set! arr 1 2)
       (output (array-ref arr 1)))) 2
((main (var arr (array 3 42))
       (array-set! arr (- 0 0) 2)
       (output (array-ref arr 1)))) 42
((main (var arr (array 3 42))
       (array-set! arr 1 (+ 1 1))
       (output (array-ref arr 0)))) 42

((main (output (array-length (array (+ 1 2) (+ 21 21)))))) 3
((main (var arr (array 3 42))
       (output (array-length arr)))) 3

((main (output (+ 12 34)))) 46
((main (output (- 34 12)))) 22
((main (output (* 12 11)))) 132
((main (output (div 13 3)))) 4
((main (output (mod 13 3)))) 1
((main (output (= 32 32)))) #t
((main (output (= 32 21)))) #f
((main (output (> 32 31)))) #t
((main (output (> 32 32)))) #f
((main (output (> 31 32)))) #f
((main (output (< 31 32)))) #t
((main (output (< 31 31)))) #f
((main (output (< 32 31)))) #f
((main (output (>= 32 32)))) #t
((main (output (>= 33 32)))) #t
((main (output (>= 33 34)))) #f
((main (output (<= 31 32)))) #t
((main (output (<= 32 32)))) #t
((main (output (<= 32 31)))) #f
((main (output (+ (+ 1 2) 3)))) 6

((main (output (not #t)))) #f
((main (output (not #f)))) #t
((main (output (not (= 31 31))))) #f
((main (output (not (= 32 21))))) #t

((main (if #t (output 11) (output 22)))) 11
((main (if #f (output 11) (output 22)))) 22
((main (if #t
           (seq (var a 1)
                (output a))
           (seq (var a 2)
                (output a))))) 1

((main
  (var i (+ 4 2))
  (set! i (+ 1 2))
  (output i))) 3

((main
  (var sum 0)
  (var i 0)
  (while (< i 3)
    (set! i (+ i 1))
    (set! sum (+ sum i)))
  (output sum))) 6

((main
  (var x 33)
  (var i 0)
  (while (< i 3)
    (set! i (+ i 1))
    (var x i))
  (output x))) 33

((main
  (var sum 0)
  (var i 0)
  (while (< i 100)
    (set! i (+ i 1))
    (set! sum (+ sum i)))
  (output sum))) 5050

((main (f))
 (defun (void f) (output 42))) 42

((main
  (var i (f 12 32))
  (output i))
 (defun (int f (int x) (int y))
   (return (+ x y)))) 44

((main
  (var i (g (f 12 32)))
  (output i))
 (defun (int f (int x) (int y))
   (return (+ x y)))
 (defun (int g (int x))
   (return (+ 11 x)))) 55

((main
  (var i (gcd 144 12144))
  (output i))
 (defun (int gcd (int a) (int b))
   (if (= a 0)
       (return b)
       (return (gcd (mod b a) a))))) 48

((main
  (var i (gcd 144 12144))
  (output i))
 (defun (int gcd (int a) (int b))
   (while (not (= a 0))
     (var t a)
     (set! a (mod b a))
     (set! b t))
   (return b))) 48

((main
  (var b (even? 42))
  (output b))
 (defun (bool even? (int n))
   (if (= n 0)
       (return #t)
       (return (odd? (- n 1)))))
 (defun (bool odd? (int n))
   (if (= n 0)
       (return #f)
       (return (even? (- n 1)))))) #t

((main (output (f)))
 (defun (int f)
   (return 1)
   (return 2))) 1

((main (return 1))) ,error:return-outside-function?
((main (output i))) ,error:unbound?
((main (var i 0) (set! i #t))) ,error:type-checking?
((main (f 1))
 (defun (bool f (bool b))
   (return b))) ,error:type-checking?
((main (if (f 1) (output 1) (output 2)))
 (defun (int f (int i)) (return i))) ,error:type-checking?
((main (output (+ 1 #t)))) ,error:type-checking?
((main (output (not 1)))) ,error:type-checking?

((main (f 1 2))
 (defun (int f (int x)) (return x))) ,error:argsnum-not-match?

((main (var i 0) (f))
 (defun (void f) (output i))) ,error:unbound?

((main (f))
 (defun (int f) (output 0))) ,error:type-checking?

((main (f))
 (defun (void f) (return 1))) ,error:type-checking?

((main (f))
 (defun (int f) (return #t))) ,error:type-checking?

    ))

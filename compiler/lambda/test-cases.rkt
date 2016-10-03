#lang racket

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
          (let* ((ret (interp exp1))
                 (correct (eqv? ret ans)))
            (display ">> ")
            (pretty-print ret)
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

12 13 ; being wrong for test
(- 12 -13) 25
(- 32 23) 9
(- 3 (- 2 1)) 2
(- (- 1 2) 3) -4

(zero? 0) #t
(zero? 1) #f

(- ((func (x) (- x 1)) 3) 2) 0

((func (x) (- x 1)) 22) 21
(((func (x) x) (func (y) y)) 11) 11

(zero? 0) #t
(zero? 1) #f
(if (zero? 0) 1 2) 1
(if (zero? 1) 1 2) 2
(- 3 (if (zero? 0) 1 2)) 2

((func (f) ((f f) 3))
 (func (p)
   (func (n)
     (if (zero? n)
         0
         (- 2 (- 0 ((p p) (- n 1)))))))) 6

(((func (f)
    ((func (x) (f (x x)))
     (func (x) (f (func (v) ((x x) v))))))
  (func (f)
    (func (n)
      (if (zero? n)
          0
          (- 2 (- 0 (f (- n 1))))))))
 4) 8

((((func (f)
     ((func (x) (f (x x)))
        (func (x) (f (func (v1)
                       (func (v2)
                         (((x x) v1) v2)))))))
    (func (f)
      (func (n)
        (func (acc)
          (if (zero? n)
              acc
              ((f (- n 1)) (- acc -2)))))))
  4) 0) 8

((func (a b) (- a b)) 12 9) 3

(((func (f)
    ((func (x) (f (x x)))
     (func (x) (f (func (v1 v2) ((x x) v1 v2))))))
  (func (f)
    (func (n acc)
      (if (zero? n)
          acc
          (f (- n 1) (- acc -2))))))
 4 0) 8

(let ((a 1))
  (- a -2)) 3

(let ((f (func (p)
           (func (n)
             (if (zero? n)
                 0
                 (- 2 (- 0 ((p p) (- n 1)))))))))
  ((f f) 4)) 8
    ))

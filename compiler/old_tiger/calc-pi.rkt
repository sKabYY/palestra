#lang racket

(define src
  '((main
     (var a 10000)
     (var c 2800)
     (var f (array 2801 0))
     (var d 0)
     (var e 0)
     (var b 0)
     (var g 0)
     (var i 0)
     (var j 0)
     (while (< i c)
       (array-set! f i (* 2 (div a 10)))
       (set! i (+ i 1)))
     (while (> c 0)
       (set! d 0)
       (set! g (- (+ (* c 2) 1) 2))
       (set! b c)
       (while (> b 0)
         (set! d (* d b))
         (set! d (+ d (* a (array-ref f b))))
         (array-set! f b (mod d g))
         (set! d (div d g))
         (set! g (- g 2))
         (set! b (- b 1)))
       (output (+ e (div d a)))
       (set! e (mod d a))
       (set! c (- c 14))))))

(require "interp.rkt")
(require "error.rkt")
(set-output! (lambda (v)
               (let ((d (->imp-value v)))
                 (cond
                   [(< d 10) (display "000") (display d)]
                   [(< d 100) (display "00") (display d)]
                   [(< d 1000) (display "0") (display d)]
                   [(< d 10000) (display d)]
                   [else (display (remainder d 10000))]))))
(with-handlers [(error:interp?
                 (lambda (e)
                   (displayln (error:interp-message e))))]
  (interp src))

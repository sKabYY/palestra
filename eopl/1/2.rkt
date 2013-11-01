#lang eopl

(#%require "lib.rkt")

(define (show-n n generator . as)
  (define (iter n as)
    (displayln as)
    (if (> n 0)
      (iter
        (- n 1)
        (apply generator as))
      (newline)))
  (iter n as))

(define (g1 n k) (list (+ n 1) (+ k 7)))
(define (g2 n k) (list (+ n 1) (* k 2)))
(define (g3 n i j) (list (+ n 1) j (+ i j)))
(define (g4 n i j) (list (+ n 1) (+ i 2) (+ i j)))

(show-n 10 g1 0 1)
(show-n 10 g2 0 1)
(show-n 10 g3 0 0 1)
(show-n 10 g4 0 1 0)

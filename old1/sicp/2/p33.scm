(load "lib.scm")

(define (map0 p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              '()
              sequence))

(define (append0 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length0 sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))

(display (map0 (lambda (x) (* x x)) (list 1 2 3)))
(newline)
(display (append0 (list 1 2 3) (list 4 5 6)))
(newline)
(display (length0 (list 1 2 3 4 5)))
(newline)

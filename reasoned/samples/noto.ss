(load "mk.ss")
(load "mklib.ss")
(load "testlib.ss")

(define-syntax noto
  (syntax-rules ()
    [(_ g* ...)
     (conda
       [g* *u] ...
       [*s])]))

(test "1"
  (run* (x y)
    (== x 1)
    (== y 2)
    (noto (== x y)))
  '((1 2)))

(test "2"
  (run* (x y)
    (noto (== x y))
    (== x 1)
    (== y 2))
  '())

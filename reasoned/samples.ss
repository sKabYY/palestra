(load "mk.ss")
(load "testlib.ss")

(test
  (run* (q) (== q #t))
  '(#t))

(test
  (run* (q) (== q #t) *s)
  '(#t))

(test
  (run* (q) (== q #t) (== q #f))
  '())

(test
  (run* (q) (== q #t) *u)
  '())

(test
  (run* (q)
    (fresh (x y)
      (== x y)
      (== y 'b)
      (== q (list x y))))
  '((b b)))

(test
  (run* (q)
    (fresh (x)
      (== x q)
      (== x 1)
      (fresh (x)
        (== x 2))))
  '(1))

(test
  (run* (x y z)
    (== x y)
    (== y 'b)
    (== z x))
  '((b b b)))

(test
  (run* (q)
    (fresh (x y)
      (conde
        [(== x 'a) (== y 1)]
        [(== x 'b) (== y 2)])
      (== q (list x y))))
  '((a 1) (b 2)))

(test
  (run* (x y)
    (conde
      [(== x 'a) (== y 1)]
      [(== x 'b) (== y 2)]))
  '((a 1) (b 2)))

(test
  (run* (x y) *s)
  '((_.0 _.1)))

(test
  (run* (x y) (== x y))
  '((_.0 _.0)))

(test
  (run* (y z)
    (fresh (x)
      (== x y)
      (fresh (x)
        (== x z))))
  '((_.0 _.1)))

(test
  (run* (y z)
    (fresh (x)
      (== x y)
      (== x z)))
  '((_.0 _.0)))

(test
  (run* (x y)
    (== x 1)
    (fresh (a b)
      (== x (cons a b))))
   '())
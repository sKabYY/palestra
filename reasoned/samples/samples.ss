(load "mklib.ss")
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
  (run* (x y)
    (conde [(== x 1)] [(== x 2)] [(== x 3)])
    (conde [(== y 'a)] [(== y 'b)]))
  '((1 a) (1 b) (2 a) (2 b) (3 a) (3 b)))

(test
  (run* (x y)
    (alli
      (conde [(== x 1)] [(== x 2)] [(== x 3)])
      (conde [(== y 'a)] [(== y 'b)])))
  '((1 a) (2 a) (1 b) (3 a) (2 b) (3 b)))

(test
  (run* (x y)
    (conde
      [(== x 'a) (== y 1)]
      [else (== y 2)]))
  '((a 1) (_.0 2)))

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

(test "membero"
  (run* (q)
    (membero q '(a b c))
    (membero q '(a c f)))
  '(a c))

(test "conde"
  (run* (q)
    (conde
      [(conde [(== q 1)] [(== q 2)])]
      [(conde [(== q 3)] [(== q 4)])]))
  '(1 2 3 4))

(test "condi"
  (run* (q)
    (condi
      [(conde [(== q 1)] [(== q 2)])]
      [(conde [(== q 3)] [(== q 4)])]))
  '(1 3 2 4))

(test
  (run 1 (q)
    (conde
      [(== #t q)]
      [nevero]))
  '(#t))

(test
  (run 1 (q)
    (condi
      [(== #t q)]
      [nevero]))
  '(#t))

(test "fresh"
  (run* (q)
    (conde [*s] [*s])
    (fresh (x) (== q x)))
  '(_.0 _.0))
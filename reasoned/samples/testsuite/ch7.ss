(load "bit.ss")

(test
  (run* (x y)
    (bit-xoro x y 0))
  '((0 0) (1 1)))

(test
  (run* (x y)
    (bit-xoro x y 1))
  '((1 0) (0 1)))

(test
  (run* (x y r)
    (bit-xoro x y r))
  '((0 0 0) (1 0 1) (0 1 1) (1 1 0)))

(test
  (run* (x y)
    (bit-ando x y 1))
  '((1 1)))

(test
  (run* (x y)
    (bit-ando x y 0))
  '((0 0) (1 0) (0 1)))

(test
  (run* (x y r c)
    (half-addero x y r c))
  '((0 0 0 0)
    (1 0 1 0)
    (0 1 1 0)
    (1 1 0 1)))

(test
  (run* (r c)
    (full-addero 1 1 1 r c))
  '((1 1)))

(test
  (run* (b x y r c)
    (full-addero b x y r c))
  '((0 0 0 0 0)
    (1 0 0 1 0)
    (0 1 0 1 0)
    (1 1 0 0 1)
    (0 0 1 1 0)
    (1 0 1 0 1)
    (0 1 1 0 1)
    (1 1 1 1 1)))

(test
  (run 11 (q)
    (bitso q))
  '(()
    (1)
    (0 1)
    (1 1)
    (0 0 1)
    (1 0 1)
    (0 1 1)
    (1 1 1)
    (0 0 0 1)
    (1 0 0 1)
    (0 1 0 1)))

(test
  (run 2 (q)
    (bitso q)
    (fresh (x)
      (== q `(,x 1))))
  '((0 1) (1 1)))

(test
  (run* (q)
    (fresh (x)
      (== q `(,x 1)))
    (bitso q))
  '((0 1) (1 1)))

(test
  (run* (q)
    (fresh (x)
      (== q `(,x ,x 1)))
    (bitso q))
  '((0 0 1) (1 1 1)))

(test
  (run* (q)
    (fresh (x y)
      (== q `(,x 0 ,y 1)))
    (bitso q))
  '((0 0 0 1) (1 0 0 1) (0 0 1 1) (1 0 1 1)))

(test
  (run 4 (q)
    (fresh (z)
      (== q (cons 1 z)))
    (bitso q))
  '((1) (1 1) (1 0 1) (1 1 1)))

(test
  (run 4 (q)
    (fresh (z)
      (== q (cons 0 z)))
    (bitso q))
  '((0 1) (0 0 1) (0 1 1) (0 0 0 1)))

(test "parse-res"
  (parse-res
    (run 10 (q) (bitso q)))
  '(0 1 2 3 4 5 6 7 8 9))

(test
  (parse-res
    (run 10 (q)
      (fresh (z)
        (== q `(1 0 . ,z)))
      (bitso q)))
  '(5 9 13 17 21 25 29 33 37 41))

(test
  (run* (q)
    (poso '(1 0 1))
    (== #t q))
  '(#t))

(test
  (run* (s)
    (addero 0 '(1) '(1) s))
  '((0 1)))

(test
  (run* (s)
    (addero 0 '(1 1) '(0 1) s))
  '((1 0 1)))

(test
  (run* (x y)
    (addero 0 x y '(1 1)))
  '(((1 1) ())
    (() (1 1))
    ((1) (0 1))
    ((0 1) (1))))

(test
  (run* (x y)
    (addero 0 x y '(1 0 1)))
  '(((1 0 1) ())
    (() (1 0 1))
    ((1) (0 0 1))
    ((0 0 1) (1))
    ((1 1) (0 1))
    ((0 1) (1 1))))

(test
  (run* (q)
    (-o '(0 0 0 1) '(1 0 1) q))
  '((1 1)))

(test
  (run* (q)
    (-o '(1 0 1) '(0 0 0 1) q))
  '())

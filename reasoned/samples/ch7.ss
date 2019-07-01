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
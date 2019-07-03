
(test
  (run 1 (x) (==^ (list x) x))
  '())

(test
  (run 1 (x)
    (fresh (y z)
      (== x z)
      (== (list 'a 'b z) y)
      (==^ x y)))
  '())

(test
  (run 1 (x)
    (fresh (y z)
      (== x z)
      (==^ (list 'a 'b z) y)
      (== x 'hah)))
  '(hah))

(load "mklib.ss")
(load "testlib.ss")

(test
  (run* (x)
    (conda
      [(== 'olive x) *s]
      [(== 'oil x) *s]
      [else *u]))
  '(olive))

(test
  (run* (x)
    (conda
      [(== 'virgin x) *u]
      [(== 'olive x) *s]
      [(== 'oil x) *s]
      [else *u]))
  '())

(test
  (run* (q)
    (fresh (x y)
      (== 'split x)
      (== 'pea y)
      (conda
        [(== 'split x) (== x y)]
        [else *s]))
    (== #t q))
  '())

(test
  (run* (q)
    (fresh (x y)
      (== 'split x)
      (== 'pea y)
      (conda
        [(== x y) (== 'split x)]
        [else *s]))
    (== #t q))
  '(#t))

(test
  (run* (q)
    (condu
      [alwayso *s]
      [else *u])
    (== #t q))
  '(#t))
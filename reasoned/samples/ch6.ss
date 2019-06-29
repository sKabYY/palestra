(load "mklib.ss")
(load "testlib.ss")

(test "always once"
  (run 1 (q)
    alwayso
    (== #t q))
  '(#t))

(test "always 5 times"
  (run 5 (q)
    alwayso
    (== #t q))
  '(#t #t #t #t #t))

(test "always with conde"
  (run 4 (q)
    (conde
      [(== #f q) alwayso]
      [else (== #t q)]))
  '(#f #f #f #f))

(test "always with condi"
  (run 4 (q)
    (condi
      [(== #f q) alwayso]
      [else (== #t q)]))
  '(#f #t #f #f))

(test
  (run 1 (q)
    (condi
      [(== #f q) alwayso]
      [else (== #t q)])
    (== #t q))
  '(#t))

(test
  (run 5 (q)
    (condi
      [(== #f q) alwayso]
      [else (anyo (== #t q))])
    (== #t q))
  '(#t #t #t #t #t))

(test
  (run 4 (q)
    (all
      (conde
        [(== #f q) *s]
        [else (== #t q)])
      alwayso)
    (== q q))
  '(#f #f #f #f))

(test
  (run 4 (q)
    (all
      (condi
        [(== #f q) *s]
        [else (== #t q)])
      alwayso)
    (== q q))
  '(#f #f #f #f))

(test
  (run 4 (q)
    (alli
      (conde
        [(== #f q) *s]
        [else (== #t q)])
      alwayso)
    (== #t q))
  '(#t #t #t #t))
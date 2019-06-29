(load "mk.ss")

(define (dummy v) v)

(define (coloro c)
  (conde
    [(== c 'red)]
    [(== c 'green)]
    [(== c 'blue)]))

(define (noto g)
  (conda
    [g *u]
    [else *s]))

(pretty-print
  (run* (A B C D E)
    (coloro A)
    (coloro B)
    (coloro C)
    (coloro D)
    (coloro E)
    (noto (== A B))
    (noto (== A C))
    (noto (== A D))
    (noto (== A E))
    (noto (== B C))
    (noto (== C D))
    (noto (== D E))))
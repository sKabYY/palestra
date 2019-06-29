(load "mk.ss")

(define (conso a d l)
  (== (cons a d) l))

(define (caro l a)
  (fresh (d)
    (conso a d l)))

(define (cdro l d)
  (fresh (a)
    (conso a d l)))

(define (nullo l) (== l '()))

(define (membero x l)
  (conde
    [(nullo l) *u]
    [(caro l x) *s]
    [(fresh (d)
       (cdro l d)
       (membero x d))]))

(define (anyo g)
  (conde [g *s] [(anyo g)]))

(define nevero (anyo *u))
(define alwayso (anyo *s))
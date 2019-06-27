(load "mk.scm")

(define (conso a d l)
  (== (cons a d) l))

(define (caro l a)
  (fresh (d)
    (conso a d l)))

(define (cdro l d)
  (fresh (a)
    (conso a d l)))

(define (nullo l) (== l '()))
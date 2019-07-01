(load "mk.ss")

(define (build-num n
  (cond
    [(zero? n) '()]
    [(odd? n) (cons 1 (div n 2))]
    [else (cons 0 (div n 2))])))

(define (bito x)
  (conde
    [(== x 0)]
    [(== x 1)]))

(define (bit-xoro x y r)
  (conde
    [(== 0 x) (== 0 y) (== 0 r)]
    [(== 1 x) (== 0 y) (== 1 r)]
    [(== 0 x) (== 1 y) (== 1 r)]
    [(== 1 x) (== 1 y) (== 0 r)]))

(define (bit-ando x y r)
  (conde
    [(== 0 x) (== 0 y) (== 0 r)]
    [(== 1 x) (== 0 y) (== 0 r)]
    [(== 0 x) (== 1 y) (== 0 r)]
    [(== 1 x) (== 1 y) (== 1 r)]))

(define (half-addero x y r c)
  (all
    (bit-xoro x y r)
    (bit-ando x y c)))

(define (full-addero b x y r c)
  (fresh (w xy wz)
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))
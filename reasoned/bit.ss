(load "mk.ss")

(define (build-num n)
  (cond
    [(zero? n) '()]
    [(odd? n) (cons 1 (div n 2))]
    [else (cons 0 (div n 2))]))

(define (bits->int bits)
  (if (null? bits)
    0
    (+ (car bits) (* 2 (bits->int (cdr bits))))))

(define (parse-a-sln sln)
  (cond
    [(bits? sln) (bits->int sln)]
    [(list? sln)
     (map parse-a-sln sln)]
    [else sln]))

(define (parse-res res) (map parse-a-sln res))

(define (bit? x) (and (number? x) (or (= x 0) (= x 1))))

(define (bits? x)
  (null? (filter (lambda (b) (not b)) (map bit? x))))

(define (bito x)
  (conde
    [(== x 0)]
    [(== x 1)]))

(define (bitso x)
  (letrec ([nozeroo (lambda (n)
                      (conde
                        [(== n '(1))]
                        [(fresh (a d)
                           (conso a d n)
                           (nozeroo d)
                           (bito a))]))])
    (conde
      [(nullo x)]
      [(nozeroo x)])))

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

(define (poso n)
  (fresh (a d)
    (== `(,a . ,d) n)))

(define (>1o n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

(define (addero d n m r)
  (condi
    [(== 0 d) (== '() m) (== n r)]
    [(== 0 d) (== '() n) (poso m) (== m r)]
    [(== 1 d) (== '() m) (addero 0 n '(1) r)]
    [(== 1 d) (== '() n) (poso m) (addero 0 '(1) m r)]
    [(== '(1) n) (== '(1) m)
     (fresh (a c)
       (== `(,a ,c) r)
       (full-addero d 1 1 a c))]
    [(== '(1) n) (gen-addero d n m r)]
    [(== '(1) m) (>1o n) (>1o r) (addero d '(1) n r)]
    [(>1o n) (gen-addero d n m r)]))

(define (gen-addero d n m r)
  (fresh (a b c e x y z)
    (== `(,a . ,x) n)
    (== `(,b . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (alli
      (full-addero d a b c e)
      (addero e x y z))))

(define (+o n m k) (addero 0 n m k))
(define (-o n m k) (+o m k n))
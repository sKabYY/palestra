(load "lib.scm")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(display (dot-product (list 1 2 3) (list 3 4 5)))
(newline)

(define (matrix-*-vector m v)
  (map
    (lambda (x) (dot-product x v))
    m))

(display (matrix-*-vector
           (list
             (list 1 2 3)
             (list 2 3 4))
           (list 2 2 1)))
(newline)

(define (transpose mat)
  (accumulate-n cons '() mat))

(display (transpose
           (list
             (list 1 2 3)
             (list 2 3 4))))
(newline)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
      (lambda (x) (matrix-*-vector cols x))
      m)))

(display (matrix-*-matrix
           (list
             (list 1 2 3)
             (list 2 3 4))
           (list
             (list 2 2 1)
             (list 0 2 1)
             (list 0 0 1))))
(newline)

(load "lib.scm")

(define empty-board '())

(define (safe1? ax ay bx by)
  (not (or (= ax bx)
           (= ay by)
           (= (abs (- ax bx)) (abs (- ay by))))))

(define (safe? k positions)
  (define (safe-iter? k ky x rest)
    (cond ((= x 0)
           #t)
          ((safe1? k ky x (car rest))
           (safe-iter? k ky (- x 1) (cdr rest)))
          (else
            #f)))
  (safe-iter? k (car positions) (- k 1) (cdr positions)))

(define (adjoin-position new-row k rest-of-queens)
  (if (null? rest-of-queens)
    (list new-row)
    (cons new-row rest-of-queens)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (print-list l)
  (if (null? l)
    (void)
    (begin
      (display (car l))
      (newline)
      (print-list (cdr l)))))

(print-list (queens 8))

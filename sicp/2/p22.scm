(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items '()))

(display (square-list '(1 2 3 4 5)))
(newline)

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items '()))

(display (square-list2 (list 1 2 3 4 5)))
(newline)

(define (square-list0 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (append answer
                    (cons (square (car things)) '())))))
  (iter items '()))

(display (square-list0 '(1 2 3 4 5)))
(newline)

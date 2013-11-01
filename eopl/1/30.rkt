#lang eopl

(#%require "lib.rkt")

(define mkpair cons)
(define fst car)
(define snd cdr)

(define (partition pred loi)
  (if (null? loi)
    (mkpair '() '())
    (let ((i (car loi))
          (rest (partition pred (cdr loi))))
      (if (pred i)
        (mkpair (cons i (fst rest)) (snd rest))
        (mkpair (fst rest) (cons i (snd rest)))))))

(define (sort/predicate pred loi)
  (if (null? loi)
    '()
    (let* ((i (car loi))
           (ls (partition (lambda (n) (pred n i)) (cdr loi))))
      (append
        (sort/predicate pred (fst ls))
        (cons i (sort/predicate pred (snd ls)))))))

(run-disp
  (sort/predicate < '(8 2 5 2 3))
  (sort/predicate > '(8 2 5 2 3)))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (enumerate-interval a b)
  (if (> a b)
    '()
    (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list j i))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (qsort l)
  (if (null? l)
    l
    (append
      (qsort (filter
               (lambda (x) (< x (car l)))
               (cdr l)))
      (cons (car l)
            (qsort (filter
                     (lambda (x) (>= x (car l)))
                     (cdr l)))))))

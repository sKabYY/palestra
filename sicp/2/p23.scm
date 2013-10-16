(define (for-each0 proc l)
  (if (null? l)
    (void)
    (begin
      (proc (car l))
      (for-each0 proc (cdr l)))))

(for-each0 (lambda (x) (display x) (newline))
           (list 57 321 88))

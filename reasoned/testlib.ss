(define-syntax test
  (syntax-rules ()
    [(_ e ans)
     (begin
       (pretty-print 'e)
       (let ([v e])
         (if (equal? v ans)
           (begin
             (printf "Get: ")
             (pretty-print v)
             (newline))
           (begin
             (printf "ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!~n")
             (printf "Expect: ")
             (pretty-print ans)
             (printf "But get: ")
             (pretty-print v)
             (newline)))))]
    [(_ name e ans) (test e ans)]))
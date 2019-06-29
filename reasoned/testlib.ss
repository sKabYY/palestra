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
             (raise 'terminated)
             (newline)))))]
    [(_ name e ans) (begin (printf "Test: ~a~n" name) (test e ans))]))
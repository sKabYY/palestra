
(trace-define-syntax comb2
  (lambda (x)
    (syntax-case x ()
      [(_ f v1 v2) #'(list (f v1 v2))]
      [(_ f v v* ...)
       #``(,(f v v*) ... . ,#,(#'(comb2 f v* ...)))])))

(trace-define-syntax bala-add
  (lambda (x)
    (syntax-case x ()
      [(_ v1 v2 v* ...)
       #`(+ #,(let ([t #'(comb2 + v1 v2 v* ...)]) t))])))

(pretty-print (bala-add 1 2 3 4))
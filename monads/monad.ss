(define-syntax do/m
  (syntax-rules (<-)
    [(_ bind e) e]
    [(_ bind (v <- e0) e e* ...)
     (bind e0 (lambda (v)
                (do/m bind e e* ...)))]
    [(_ bind e0 e e* ...)
     (bind e0 (lambda (_)
                (do/m bind e e* ...)))]))

#lang racket
(provide macro-expand)

(define (>> expr)
  (match expr
    [(? symbol? s) s]
    [(? number? n) n]
    [(? boolean? b) b]
    [`(func ,args ,body)
     (cond
       [(null? args) (error "[Error] func must have at least one argument")]
       [(null? (cdr args)) `(func ,args ,(>> body))]
       [else `(func (,(car args)) ,(>> `(func ,(cdr args) ,body)))])]
    [`(let ,decs ,body)
     (if (null? decs)
         (>> body)
         (let* ((dec (car decs))
                (rest-decs (cdr decs))
                (a (car dec))
                (e (cadr dec)))
           `((func (,a)
                   ,(>> `(let ,rest-decs ,body)))
             ,(>> e))))]
    [`(if ,e1 ,e2, e3) `(if ,(>> e1) ,(>> e2) ,(>> e3))]
    [`(- ,e1 ,e2) `(- ,(>> e1) ,(>> e2))]
    [`(zero? ,e1) `(zero? ,(>> e1))]
    [apps
     (if (list? apps)
         (let ((l (length apps)))
           (cond
             [(= l 0) (error "[Error] unknown symbol: ()")]
             [(= l 1) (error "[Error] application must have at least one argument")]
             [(= l 2) (map >> apps)]
             [else (>> `((,(car apps) ,(cadr apps)) . ,(cddr apps)))]))
         apps)]))

(define macro-expand >>)

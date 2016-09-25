(load "monad.ss")

(define (return-writer a)
  `(,a . ()))

(define (bind-writer ma f)
  (let ([mb (f (car ma))])
    `(,(car mb) . ,(append (cdr ma) (cdr mb)))))

(define (tell-writer to-write)
  `(_ . (,to-write)))


(define (reciprocals l)
  (cond
    [(null? l) (return-writer '())]
    [(zero? (car l))
     (do/m bind-writer
           (tell-writer "Saw a 0")
           (reciprocals (cdr l)))]
    [else
     (do/m bind-writer
           (d <- (reciprocals (cdr l)))
           (return-writer (cons (/ 1 (car l)) d)))]))


(pretty-print (reciprocals '(0 1 0 2)))

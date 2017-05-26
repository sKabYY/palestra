(load "match.ss")
(load "monad.ss")

(define (just a) `(Just ,a))
(define nothing 'Nothing)

(define return just)
(define (>>= ma f)
  (match ma
    [(Just ,[f -> mb]) mb]
    [,x (guard (eq? ma nothing)) nothing]))

(define (inv x)
  (if (zero? x) nothing (return (/ 1.0 x))))

(pretty-print (>>= (just 10) inv))
(pretty-print (>>= (just 0) inv))
(pretty-print (>>= nothing inv))

(pretty-print (>>= (>>= (just 10) inv) inv))

(pretty-print
 (do/m >>=
       (s <- (just 10))
       (s <- (inv s))
       (s <- (inv s))
       (return s)))

(load "match.ss")
(define (empty) '(empty))
(define (epsilon) '(epsilon))
(define (t value) `(t ,value))
(define (delta lang) `(delta ,lang))

; or
(define (u this that) `(u ,this ,that))
; cat
(define (o left right) `(o ,left ,right))
; star
(define (x lang) `(x ,lang))

(define (parse reg)
  (match reg
    [(: ,[r1] ,[r2]) (u r1 r2)]
    [(: ,[r1] ,r2 ,r3 ...)
     (u r1 (parse `(: ,r2 ,r3 ...)))]
    [(* ,r1 ...) (x (parse `(,r1 ...)))]
    [(,[r1]) r1]
    [(,[r1] ,r2 ...)
     (o r1 (parse `(,r2 ...)))]
    [,x (t x)]))

(define (D c L)
  (match L
    [(empty) (empty)]
    [(epsilon) (empty)]
    [(delta ,_) (empty)]
    [(t ,a) (if (eqv? a c) (epsilon) (empty))]
    [(u ,L1 ,L2) (u (D c L1) (D c L2))]
    [(x ,L1) (o (D c L1) L)]
    [(o ,L1 ,L2) (u (o (delta L1) (D c L2))
                  (o (D c L1) L2))]))

(define (nullable? L)
  (match L
    [(empty) #f]
    [(epsilon) #t]
    [(t ,_) #f]
    [(x ,_) #t]
    [(delta ,L1) (nullable? L1)]
    [(u ,L1 ,L2) (or (nullable? L1) (nullable? L2))]
    [(o ,L1 ,L2) (and (nullable? L1) (nullable? L2))]))

(define (recognizes? w p)
  (cond [(null? w) (nullable? p)]
        [else (recognizes? (cdr w) (D (car w) p))]))

(define (test w p)
  (let ([r (parse p)])
    (printf "=~a w=~a p=~a~n" (recognizes? w r) w p)))

(test '(a a a) '(a (: a b) a))
(test '(a b a) '(a (: a b) a))
(test '(a) '(a (: a b) a))
(test '(x x x) '(* x))
(test '() '(* x))

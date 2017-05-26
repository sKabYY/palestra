(load "match.ss")
(load "monad.ss")

(define (Some x xs) `(Some ,x ,xs))
(define None 'None)
(define (None? x) (eq? x None))

(define-syntax --
  (syntax-rules (Some None)
    [(_ r
        [None none-body ...]
        [(Some x xs) some-body ...])
     (if (None? r)
         (begin none-body ...)
         (match r
           [(Some ,x ,xs)
            (begin some-body ...)]))]))

(define (@or p q)
  (lambda (s)
    (-- (p s)
        [None (q s)]
        [(Some x xs) (Some x xs)])))

(define (>>= m f)
  (lambda (s)
    (-- (m s)
        [None None]
        [(Some x xs) ((f x) xs)])))

(define (return x)
  (lambda (s) (Some x s)))

(define (@cat p ps)
  (do/m >>=
        (x <- p)
        (xs <- ps)
        (return (cons x xs))))

(define (@? p v) (@or p (return v)))
(define (@* p)
  (@? (do/m >>=
            (x <- p)
            (xs <- (@* p))
            (return (cons x xs)))
      '()))
(define (@+ p) (@cat p (@* p)))
(define (@... ps)
  (if (null? ps)
      (return '())
      (@cat (car ps) (@... (cdr ps)))))

(define (satisfy pred?)
  (lambda (s)
    (cond
      [(null? s) None]
      [(pred? (car s)) (Some (car s) (cdr s))]
      [else None])))

(define (range a b)
  (satisfy (lambda (x) (and (char>=? x a) (char<=? x b)))))

(define (exactly x)
  (satisfy (lambda (x0) (char=? x x0))))

(define digit (range #\0 #\9))
(define lower (range #\a #\z))
(define upper (range #\A #\Z))
(define letter (@or lower upper))
(define space (exactly #\space))
(define spaces (@+ space))

(define (token str)
  (do/m >>=
        (@... (map exactly (string->list str)))
        (return str)))

(define name
  (do/m >>=
        spaces
        (x <- (@+ letter))
        (@* space)
        (return (list->string x))))

(define (test p str)
  (pretty-print (p (string->list str))))

(test digit "23a")
(test lower "asd2")
(test letter "asd2")
(test (token "if") "if12")
(test (do/m >>=
            (token "if")
            (n1 <- name)
            (token "then")
            (n2 <- name)
            (return `(if-then ,n1 ,n2)))
      "if xxx then yyy")

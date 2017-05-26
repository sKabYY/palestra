(load "monad.ss")

(define (return-state a)
  (lambda (s) `(,a . ,s)))

(define (bind-state ma f)
  (lambda (s)
    (let ([vs (ma s)])
      (let ([v (car vs)]
            [s^ (cdr vs)])
        ((f v) s^)))))

(define get-state
  (lambda (s) `(,s . ,s)))

(define (put-state new-s)
  (lambda (s) `(,(void) . ,new-s)))


(define (even-length? l)
  (cond
    [(null? l) (return-state '_)]
    [else
     (do/m bind-state
           (s <- get-state)
           (put-state (not s))
           (even-length? (cdr l)))]))

(printf "v1: ")
(pretty-print ((even-length? '(1 2 3 4)) #t))


(define (isum n i)
  (cond
    [(> i n) (return-state 0)]
    [else
     (do/m bind-state
           (s <- get-state)
           (put-state (not s))
           (e <- (return-state (if s i (- i))))
           (r <- (isum n (+ i 1)))
           (return-state (+ e r)))]))

(printf "v2: ")
(pretty-print ((isum 5 1) #t))

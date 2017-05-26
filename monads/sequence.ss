(load "monad.ss")

; M number :: number -> (number * number)
;             old-state -> (value * new-state)

(define (return i)
  (lambda (s) (cons i s)))

(define (>>= m f)
  (lambda (s)
    (let ([p (m s)])
      ((f (car p)) (cdr p)))))

(define (get-state)
  (lambda (s) (cons s s)))

(define (set-state s)
  (lambda (old-s) (cons (void) s)))

(define (sequence0)
  (>>= (get-state)
       (lambda (state)
         (>>= (set-state (+ state 1))
              (lambda (_)
                (return (+ state 1)))))))

(define (sequence1)
  (do/m >>=
        (state <- (get-state))
        (set-state (+ state 1))
        (return (+ state 1))))

(define (sequence)
  (do/m >>=
        (i <- (get-state))
        (let ([ans (+ i 1)])
          (do/m >>=
                (set-state ans)
                (return ans)))))

(define (printi v) (return (pretty-print v)))

;(define sequence sequence0)
(define run-program
  (do/m >>=
        (i1 <- (sequence))
        (i2 <- (sequence))
        (printi i1)
        (printi i2)
        (i3 <- (sequence))
        (printi i3)))

(run-program 0)

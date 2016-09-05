#lang racket

(provide (combine-out throw put))


(define (throw err . args)
  (raise (err (apply string-append
                     (map (lambda (a) (format "~a" a))
                          args)))))


(define (put . ss)
  (let loop ([ss ss])
    (cond
      [(null? ss)
       (void)]
      [(eq? (car ss) "~n")
       (printf "~n")
       (loop (cdr ss))]
      [else
       (printf "~a" (car ss))
       (loop (cdr ss))])))

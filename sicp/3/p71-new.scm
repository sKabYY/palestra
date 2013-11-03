(load "weighted-pairs-new.scm")

; failed! TODO

(define (sumofexpt3 i j)
  (+ (expt i 3) (expt j 3)))

(define ipairs
  (weighted-pairs-new integers integers sumofexpt3))

(define (filter-the-same stream)
  (define (iter prev prev-count s)
    (cond ((= prev (stream-car s))
           (iter prev (+ prev-count 1) (stream-cdr s)))
          ((= prev-count 1)
           (iter (stream-car s) 1 (stream-cdr s)))
          (else
            (cons
              prev
              (delay
                (iter (stream-car s) 1 (stream-cdr s)))))))
  (iter (stream-car stream) 1 (stream-cdr stream)))

(define sums (stream-map-0 (lambda (x) (apply sumofexpt3 x)) ipairs))
(define ramanujan (filter-the-same sums))

(stream-for-n println ramanujan 6)

; (fake-define (cons-stream a b)
;   (cons a (delay b)))

(define the-empty-stream '())
(define (stream-null? s) (null? s))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-ref-0 s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref-0 (stream-cdr s) (- n 1))))

(define (stream-map-0 proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons (proc (car s))
          (delay (stream-map-0 proc (stream-cdr s))))))

(define (stream-map-n proc . streams)
  (if (stream-null? (car streams))
    the-empty_stream
    (cons
      (apply proc (map stream-car streams))
      (delay (apply stream-map-n
                    proc
                    (map stream-cdr streams))))))

(define (stream-filter-0 predicate s)
  (cond ((stream-null? s)
         the-empty-stream)
        ((predicate (stream-car s))
         (cons (stream-car s)
               (delay (stream-filter-0 predicate (stream-cdr s)))))
        (else
          (stream-filter-0 predicate (stream-cdr s)))))

(define (stream-for-n proc s n)
  (if (= n 0)
    'done
    (begin
      (proc (stream-car s))
      (stream-for-n proc (stream-cdr s) (- n 1)))))

(define (integers-starting-from n)
  (cons n
        (delay (integers-starting-from (+ n 1)))))

(define integers (integers-starting-from 1))

(define (add-streams . streams)
  (apply stream-map-n + streams))

(define (mul-streams . streams)
  (apply stream-map-n * streams))

(define (scale-stream stream a)
  (stream-map-n
    (lambda (x) (* x a))
    stream))

(define (partial-sums stream)
  (cons
    (stream-car stream)
    (delay (add-streams
             (partial-sums stream)
             (stream-cdr stream)))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons s1car
                         (delay (merge (stream-cdr s1) s2))))
                  ((> s1car s2car)
                   (cons s2car
                         (delay (merge s1 (stream-cdr s2)))))
                  (else
                    (cons s1car
                          (delay (merge
                                   (stream-cdr s1)
                                   (stream-cdr s2))))))))))

(define (println x)
  (begin
    (display x)
    (newline)))

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref-0 s 0))
        (s1 (stream-ref-0 s 1))
        (s2 (stream-ref-0 s 2)))
    (cons
      (- s2 (/ (square (- s2 s1))
               (+ s0 (* -2 s1) s2)))
      (delay (euler-transform (stream-cdr s))))))

(define (make-tableau transform s)
  (cons s
        (delay (make-tableau transform (transform s)))))

(define (accelerated-sequence transform s)
  (stream-map-n stream-car (make-tableau transform s)))

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons
      (stream-car s1)
      (delay (interleave s2 (stream-cdr s1))))))

(define (pairs s t)
  (cons
    (list (stream-car s) (stream-car t))
    (delay (interleave
             (stream-map-n (lambda (x) (list (stream-car s) x))
                           (stream-cdr t))
             (pairs (stream-cdr s) (stream-cdr t))))))

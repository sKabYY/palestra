(load "stream.scm")

; failed TODO

; (define (weight i j) (...))

(define (merge-weighted ps1 ps2 weight)
  (cond ((stream-null? ps1) ps2)
        ((stream-null? ps2) ps1)
        (else
          (let ((w1 (apply weight (stream-car ps1)))
                (w2 (apply weight (stream-car ps2))))
            (cond ((> w1 w2)
                   (cons (stream-car ps2)
                         (delay (merge-weighted
                                  ps1
                                  (stream-cdr ps2)
                                  weight))))
                  ((equal? (stream-car ps1) (stream-car ps2))
                   (cons (stream-car ps1)
                         (delay (merge-weighted
                                  (stream-cdr ps1)
                                  (stream-cdr ps2)
                                  weight))))
                  (else
                   (cons (stream-car ps1)
                         (delay (merge-weighted
                                  (stream-cdr ps1)
                                  ps2
                                  weight)))))))))

; ASSERT:
;   for any i, j
;     1) weight(i, j) <= weight(i+1, j)
;     2) weight(i, j) <= weight(i, j+1)
;     3) for any stream s: s[i] <= s[i+1]
(define (weighted-pairs s t weight)
  (define (get-fst . v) (car v))
  (cons
    (list (stream-car s) (stream-car t))
    (delay
      (merge-weighted
        (weighted-pairs (stream-cdr s) t weight)
        (weighted-pairs s (stream-cdr t) weight)
        weight))))

(define (sorted? pair)
  (and (pair? pair)
       (not (null? (cdr pair)))
       (< (car pair) (cadr pair))))

(define (weighted-sorted-pairs s t weight)
  (stream-filter-0
    sorted?
    (weighted-pairs s t weight)))

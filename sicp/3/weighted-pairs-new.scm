(load "stream.scm")

; The following answers come from:
; http://eli.thegreenplace.net/2007/11/10/sicp-section-353/

(define merge-count 0)

(define (merge-weighted-new s1 s2 weight)
  (set! merge-count (+ merge-count 1))
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let* ((s1car (stream-car s1))
             (s1w (apply weight s1car))
             (s2car (stream-car s2))
             (s2w (apply weight s2car)))
        (cond ((<= s1w s2w)
               (cons s1car
                     (delay
                       (merge-weighted-new (stream-cdr s1) s2 weight))))
              (else
                (cons s2car
                      (delay
                        (merge-weighted-new s1 (stream-cdr s2) weight)))))))))

(define (weighted-pairs-new s1 s2 weight)
  (cons
    (list (stream-car s1) (stream-car s2))
    (delay
      (merge-weighted-new
        (stream-map-0
          (lambda (x) (list (stream-car s1) x))
          (stream-cdr s2))
        (weighted-pairs-new (stream-cdr s1) (stream-cdr s2) weight)
        weight))))

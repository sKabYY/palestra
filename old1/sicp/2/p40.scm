(load "lib.scm")

(define (prime? x)
  (define (out? i) (> (* i i) x))
  (define (iter i)
    (cond ((out? i) x)
          ((= (remainder x i) 0) i)
          (else (iter (+ i 1)))))
  (= (iter 2) x))

(define (loop-range f low high)
  (if (> low high)
    (void)
    (begin
      (f low)
      (loop-range f (+ low 1) high))))

(display "Test prime?: ")
(loop-range
  (lambda (i)
    (if (prime? i)
      (begin
        (display i)
        (display #\ ))
      (void)))
  1
  100)
(newline)

(define (pair-sum pair)
  (+ (car pair) (cadr pair)))

(define (prime-sum? pair)
  (prime? (pair-sum pair)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (pair-sum pair)))

(define (prime-sum-pair n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(display (prime-sum-pair 6))
(newline)

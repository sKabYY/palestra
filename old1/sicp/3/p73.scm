(load "stream.scm")

(define (integral integrand initial-value dt)
  (define int
    (cons
      initial-value
      (delay (add-streams (scale-stream integrand dt)
                          int))))
  int)

(define (RC R C dt)
  (define (cir v0 i)
    (add-streams
      (scale-stream i R)
      (integral
        (scale-stream i (/ 1.0 C))
        v0
        dt)))
  cir)

(define RC1 (RC 5.0 1.0 0.5))

(define ones (cons 1 (delay ones)))

(stream-for-n
  println
  (RC1 5.0 ones)
  10)

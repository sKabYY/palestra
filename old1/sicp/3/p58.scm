(load "stream.scm")

(define (expand0 num den radix)
  (cons
    (quotient (* num radix) den)
    (delay (expand0 (remainder (* num radix) den) den radix))))

(stream-for-n
  println
  (expand0 1 7 10)
  10)

(newline)

(stream-for-n
  println
  (expand0 3 8 10)
  10)

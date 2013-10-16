(load "stream.scm")

(define global-count 0)

(define (add-streams-p57 s1 s2)
  (cons
    (begin
      (set! global-count (+ global-count 1))
      (+ (stream-car s1) (stream-car s2)))
    (delay (add-streams-p57 (stream-cdr s1) (stream-cdr s2)))))

(define fibs
  (cons
    1
    (delay (cons
             1
             (delay (add-streams-p57 fibs
                                 (stream-cdr fibs)))))))

(stream-for-n
  println
  fibs
  10)

(display "count=")(println global-count)

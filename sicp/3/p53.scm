(load "stream.scm")

(define s (cons 1 (delay (add-streams s s))))

(stream-for-n
  (lambda (x) (begin
                (display x)
                (newline)))
  s
  10)

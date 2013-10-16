(load "stream.scm")

(define factorials
  (cons
    1
    (delay (mul-streams
             factorials
             (stream-cdr integers)))))

(stream-for-n println factorials 10)

(defun count-anywhere (x c)
  (if (listp c)
      (apply #'+ (mapcar #'(lambda (c0) (count-anywhere x c0)) c))
      (if (eql x c) 1 0)))

(print (count-anywhere 'a '()))
(print (count-anywhere 'a '(a ((a) b) a)))
(print (count-anywhere 'a '(a ((a x a) b) a)))
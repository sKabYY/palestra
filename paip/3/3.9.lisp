(defun length0 (lst)
  (reduce #'+ (mapcar #'(lambda (x) 1) lst)))

(print (length0 '()))
(print (length0 '(a)))
(print (length0 '(a b)))
(print (length0 '(a b c)))
(print (length0 '(a (b1 b2) c d)))
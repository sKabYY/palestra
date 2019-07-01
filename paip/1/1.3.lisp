(defun count-atoms (x)
  (if (listp x)
      (apply #'+ (mapcar #'count-atoms x))
      (if (eql nil x) 0 1)))

(print (count-atoms '(a (b) c)))
(print (count-atoms '(a nil c)))
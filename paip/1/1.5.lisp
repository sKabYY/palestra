(defun dot-product (v1 v2)
  (apply #'+ (mapcar #'* v1 v2)))

(print (dot-product '(10 20) '(3 4)))
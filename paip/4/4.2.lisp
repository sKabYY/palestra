(defun mappend (fn lst) (apply #'append (mapcar fn lst)))

(defun permutations (lst)
  (if (null lst)
      '(())
      (mappend #'(lambda (x)
                   (let ((sublst (remove x lst :count 1 :test #'eq)))
                     (mapcar #'(lambda (l) (cons x l)) (permutations sublst))))
               lst)))

(print (permutations '(1 2 3 4)))
(print '-)
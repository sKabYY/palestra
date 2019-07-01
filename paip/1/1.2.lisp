(defun power (base n)
  (cond
    ((= n 0) 1)
    ((evenp n) ((lambda (x) (* x x)) (power base (/ n 2))))
    (t (* base (power base (- n 1))))))

(print (power 3 2))
(print (power 2 10))
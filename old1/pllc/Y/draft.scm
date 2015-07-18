Y :=
(lambda (f)
  ((lambda (x) (f (x x)))
   (lambda (x) (f (x x)))))

s_wrapper :=
(lambda (s) (lambda () (s)))

(Y s_wrapper)
= ((lambda (x) ((lambda (s) (lambda () (s))) (x x)))
   (lambda (x) ((lambda (s) (lambda () (s))) (x x))))
= ((lambda (x) ((lambda () (x x))))
   (lambda (x) ((lambda () (x x)))))
= ((lambda (x) (x x)) (lambda (x) (x x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Y :=
(lambda (f)
  ((lambda (x) (x x))
   (lambda (x) (f (lambda (v) ((x x) v))))))
= (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (x x)))))

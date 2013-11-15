Some test cases:

> \> (+ 1 1)

> \> (+ 1 2 3)

> \> (((lambda (f) (lambda (x) (f x))) (lambda (y) (+ y y))) 1)

> \> ((lambda (x) (x x)) (lambda (x) (x x)))

> \> (((lambda (f) (lambda (x) (+ (f x) (f (f x))))) (lambda (x) (+ x 1))) 1)

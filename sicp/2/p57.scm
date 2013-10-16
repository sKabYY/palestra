; deriv
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? x n)
  (and (number? x) (= x n)))

(define (generalized-product? sym x)
  (and (pair? x) (eq? (car x) sym)))

(define (generalized-multiplier p) (cadr p))

(define (make-generalized-product sym op identity zero m1 . m2)
  (define (make . m)
    (apply make-generalized-product sym op identity zero m))
  (cond ((null? m2)
         m1)
        ((and (number? zero)
              (or (=number? m1 zero) (=number? (car m2) zero)))
         0)
        ((=number? m1 identity)
         (apply make m2))
        ((=number? (car m2) identity)
         (apply make m1 (cdr m2)))
        ((and (number? m1) (number? (car m2)))
         (apply make (op m1 (car m2)) (cdr m2)))
        ((number? (car m2))
         (apply make (car m2) m1 (cdr m2)))
        ((generalized-product? sym m1)
         (apply make
                (append (cdr m1) m2)))
        ((generalized-product? sym (car m2))
         (apply make
                m1
                (append (cdar m2) (cdr m2))))
        (else
          (append (list sym m1) m2))))
(define (generalized-multiplicand op identity zero p)
  (apply make-generalized-product (car p) op identity zero (cddr p)))

(define (sum? x) (generalized-product? '+ x))
(define (addend s) (generalized-multiplier s))
(define (make-sum . a)
  (apply make-generalized-product '+ + 0 '() a))
(define (augend s)
  (generalized-multiplicand + 0 '() s))

(define (product? x) (generalized-product? '* x))
(define (multiplier p) (generalized-multiplier p))
(define (make-product . m)
  (apply make-generalized-product '* * 1 0 m))
(define (multiplicand p)
  (generalized-multiplicand * 1 0 p))

(define (exponent e) (caddr e))
(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '^)
       (number? (exponent x))))
(define (base e) (cadr e))
(define (make-exponentiation b n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) b)
        ((number? n) (list '^ b n))
        (else (error "the exponent is not number" n))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product
             (deriv (multiplier exp) var)
             (multiplicand exp))
           (make-product
             (multiplier exp)
             (deriv (multiplicand exp) var))))
        ((exponentiation? exp)
         (let ((b (base exp))
               (n (exponent exp)))
           (make-product
             n
             (make-product
               (make-exponentiation b (- n 1))
               (deriv b var)))))
        (else
          (error "unknown expression type -- DERIV" exp))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (println s)
  (begin
    (display s)
    (newline)))

(println (deriv '(+ x y) 'x))
(println (deriv '(+ (* a3 (^ x 3)) (* a2 (^ x 2)) (* a1 (^ x 1)) a0) 'x))
(println (deriv '(+ (* a2 (^ x 2)) (* a1 (^ x 1)) a0) 'x))

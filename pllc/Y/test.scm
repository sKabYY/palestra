(define (Y f)
  ((lambda (x) (x x))
   (lambda (x) (f (lambda args (apply (x x) args))))))

(define (Y0 f)
  ((lambda (x) (f (x x)))
   (lambda (x) (f (x x)))))

(define (Yv f)
  ((lambda (x) (f (x x)))
   (lambda (x) (f (lambda args (apply (x x) args))))))

(define (mkgcd gcd)
  (lambda (a b)
    (if (= a 0)
      b
      (gcd (remainder b a) a))))

(display ((Y mkgcd) 144 12144))(newline)
(display ((Yv mkgcd) 144 12144))(newline)

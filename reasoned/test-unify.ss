(load "mk.ss")

(define (build-assoc ps)
  (letrec ([rec (lambda (ps ass)
                        (dbg ass)
                        (if (null? ps)
                          ass
                          (let ([head (car ps)]
                                [rest (cdr ps)])
                            (rec rest (unify ass (car head) (cdr head))))))])
    (rec ps (make-assoc))))

(define (test-all tests)
  (if (null? tests)
    (void)
    (begin
      (dbg (build-assoc (car tests)))
      (dbg "===")
      (test-all (cdr tests)))))

(let ([a (var 'a)]
      [b (var 'b)]
      [c (var 'c)]
      [e (var 'e)]
      [x (var 'x)])
  (test-all `(
              (
                [,a . 1]
                [,b . 2]
                [(,a ,b) . ,c]
                [,x . (,a ,b ,c ,e)]
               )
              (
                [,a . 1]
                [(,a ,b) . ,c]
                [,b . 2]
                [,x . (,a ,b ,c ,e)]
               )
              (
                [(,a ,b) . ,c]
                [,b . 2]
                [,a . 1]
                [,e . ,c]
                [,x . (,a ,b ,c ,e)]
               )
              )))

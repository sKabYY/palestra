(define (dbg obj) (printf "~a~n" obj) obj)

;;; assoc

(define-structure (var name))
(define var (let ([idx 0])
              (lambda (name)
                (set! idx (+ idx 1))
                (make-var (string->symbol
                            (string-append
                              (symbol->string name) "/" (number->string idx)))))))

(define (make-assoc) '())
(define (extend-assoc ass v1 v2) (cons (cons v1 v2) ass))
(define (unify/ext ext ass v1 v2)
  (let ([v1 (walk v1 ass)]
        [v2 (walk v2 ass)])
    (cond
      [(and (pair? v1) (pair? v2) (unify/ext ext ass (car v1) (car v2)))
       => (lambda (a) (unify/ext ext a (cdr v1) (cdr v2)))]
      [(var? v1) (ext ass v1 v2)]
      [(var? v2) (ext ass v2 v1)]
      [(eqv? v1 v2) ass]
      [else #f])))
(define (unify ass v1 v2) (unify/ext extend-assoc ass v1 v2))
(define (unify^ ass v1 v2)
  (letrec ([occurs^ (lambda (ass v1 v2)
                      (cond
                        [(pair? v2) (or (occurs^ ass v1 (car v2)) (occurs^ ass v1 (cdr v2)))]
                        [(and (var? v2) (eq? v1 v2)) #t]
                        [else #f]))]
           [extend-assoc^ (lambda (ass v1 v2) (and (not (occurs^ ass v1 v2)) (extend-assoc ass v1 v2)))])
    (unify/ext extend-assoc^ ass v1 v2)))

(define (walk v ass)
  (cond
    [(pair? v) (cons (walk (car v) ass) (walk (cdr v) ass))]
    [(and (var? v) (assq v ass))
     => (lambda (p) (walk (cdr p) ass))]
    [else v]))

;;; stream

(define-structure (stream car get-cdr))
(define (empty-stream) '())
(define (stream-of e) (make-stream e (delay (empty-stream))))
(define (stream-cdr s) (force (stream-get-cdr s)))
(define (stream-null? s) (null? s))
(define (stream-map f s)
  (if (stream-null? s)
    (empty-stream)
    (make-stream (f (stream-car s)) (delay (stream-map f (stream-cdr s))))))
(define (stream-append s1 ds2)
  (if (stream-null? s1)
    (force ds2)
    (make-stream (stream-car s1) (delay (stream-append (stream-cdr s1) ds2)))))
(define (stream-interleave s1 ds2)
  (if (stream-null? s1)
    (force ds2)
    (make-stream (stream-car s1) (delay (stream-interleave (force ds2) (delay (stream-cdr s1)))))))

(define (stream-take n ds)
  (if (and n (= n 0))
    '()
    (let ([s (force ds)])
      (if (stream-null? s)
        '()
        (cons (stream-car s) (stream-take (and n (- n 1)) (delay (stream-cdr s))))))))

;;; goal
;;; typeof(goal): assoc -> stream of assoc

(define (apply-goal g) (g (make-assoc)))

(define *s (lambda (ass) (stream-of ass)))
(define *u (lambda (ass) (empty-stream)))

(define (assoc->stream ass) (if ass (*s ass) (*u ass)))

(define (== x y) (lambda (ass) (assoc->stream (unify ass x y))))
(define (==^ x y) (lambda (ass) (assoc->stream (unify^ ass x y))))

(define (join/merge merge s g)
  (if (stream-null? s)
    (empty-stream)
    (merge (g (stream-car s)) (delay (join/merge merge (stream-cdr s) g)))))

(define (join s g) (join/merge stream-append s g))

(define (joini s g) (join/merge stream-interleave s g))

(define-syntax all
  (syntax-rules ()
    [(_) *s]
    [(_ g) g]
    [(_ g gs ...) (lambda (ass) (join (g ass) (all gs ...)))]))

(define-syntax alli
  (syntax-rules ()
    [(_) *s]
    [(_ g) g]
    [(_ g gs ...) (lambda (ass) (joini (g ass) (all gs ...)))]))

(define-syntax cond/if
  (syntax-rules (else)
    [(_ ifer) *u]
    [(_ ifer [else g gs ...])
     (cond/if ifer [g gs ...])]
    [(_ ifer [g gs ...] c* ...)
     (ifer g (all gs ...) (cond/if ifer c* ...))]))

(define-syntax cond+
  (syntax-rules ()
    [(_ ifer [g gs ...] c* ...) (cond/if ifer [g gs ...] c* ...)]))

(define-syntax ife
  (syntax-rules ()
    [(_ g1 g2 g3)
     (lambda (ass)
       (stream-append (join (g1 ass) g2) (delay (g3 ass))))]))

(define-syntax conde
  (syntax-rules ()
    [(_ c* ...) (cond+ ife c* ...)]))

(define-syntax ifi
  (syntax-rules ()
    [(_ g1 g2 g3)
     (lambda (ass)
       (stream-interleave (join (g1 ass) g2) (delay (g3 ass))))]))

(define-syntax condi
  (syntax-rules ()
    [(_ c* ...) (cond+ ifi c* ...)]))

(define-syntax ifa
  (syntax-rules ()
    [(_ g1 g2 g3)
     (lambda (ass)
       (let ([s (g1 ass)])
         (if (stream-null? s)
           (g3 ass)
           (join s g2))))]))

(define-syntax conda
  (syntax-rules ()
    [(_ c* ...) (cond+ ifa c* ...)]))

(define-syntax ifu
  (syntax-rules ()
    [(_ g1 g2 g3)
     (lambda (ass)
       (let ([s (g1 ass)])
         (if (stream-null? s)
           (g3 ass)
           (g2 (stream-car s)))))]))

(define-syntax condu
  (syntax-rules ()
    [(_ c* ...) (cond+ ifu c* ...)]))

(define-syntax let-fresh
  (syntax-rules ()
    [(_ (v* ...) e e* ...)
     (let ([v* (var 'v*)] ...) e e* ...)]))

(define-syntax fresh
  (syntax-rules ()
    [(_ (v* ...) g g* ...)
     (let-fresh (v* ...) (all g g* ...))]))

;;; run

(define-syntax run
  (syntax-rules ()
    [(_ n (q) g gs ...)
     (let-fresh (q)
       (map reify
         (stream-take n
           (delay
             (stream-map (lambda (ass) (walk q ass))
               (apply-goal (all g gs ...)))))))]
    [(_ n (q qs ...) g gs ...)
     (run n (x) (fresh (q qs ...) g gs ... (== x (list q qs ...))))]))
(define-syntax run*
  (syntax-rules ()
    [(_ (q qs ...) g gs ...) (run #f (q qs ...) g gs ...)]))

(define (reify v)
  (letrec ([rec/k (lambda (v e k)
                    (cond
                      [(var? v)
                       (cond
                         [(assq v e) => (lambda (p) (k (reify-n (cdr p)) e))]
                         [else (let ([n (length e)])
                                 (k (reify-n n) (cons (cons v n) e)))])]
                      [(pair? v)
                       (rec/k (car v) e
                         (lambda (v0 e0)
                           (rec/k (cdr v) e0
                             (lambda (v1 e1)
                               (k (cons v0 v1) e1)))))]
                      [else (k v e)]))]
           [reify-n (lambda (n)
                      (string->symbol (string-append "_." (number->string n))))])
    (rec/k v '() (lambda (x e) x))))
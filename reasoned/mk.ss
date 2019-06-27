(define (dbg obj) (printf "~a~n" obj) obj)

;;; assoc

;(define-structure (var name))
;(define (var name) (make-var name))
(define-structure (var name idx))
(define var (let ([idx 0])
              (lambda (name)
                (set! idx (+ idx 1))
                (make-var name idx))))

(define (make-assoc) '())
(define (extend-assoc ass v1 v2) (cons (cons v1 v2) ass))
(define (unify ass v1 v2)
  (let ([v1 (walk v1 ass)]
        [v2 (walk v2 ass)])
    (cond
      [(and (pair? v1) (pair? v2) (unify ass (car v1) (car v2)))
       => (lambda (u) (unify u (cdr v1) (cdr v2)))]
      [(var? v1) (extend-assoc ass v1 v2)]
      [(var? v2) (extend-assoc ass v2 v1)]
      [(eqv? v1 v2) ass]
      [else #f])))
(define (walk v ass)
  (cond
    [(pair? v) (cons (walk (car v) ass) (walk (cdr v) ass))]
    [(and (var? v) (assq v ass))
     => (lambda (p) (walk (cdr p) ass))]
    [else v]))

;;; stream

(define-structure (stream fst get-rst))
(define (empty-stream) '())
(define (stream-of e) (make-stream e (delay (empty-stream))))
(define (stream-rst g) (force (stream-get-rst g)))
(define (stream-null? g) (null? g))
(define (stream-map f g)
  (if (stream-null? g)
    (empty-stream)
    (make-stream (f (stream-fst g)) (delay (stream-map f (stream-rst g))))))
(define (stream-append g1 dg2)
  (if (stream-null? g1)
    (force dg2)
    (make-stream (stream-fst g1) (delay (stream-append (stream-rst g1) dg2)))))
(define (stream-flatten ss)
  (if (stream-null? ss)
    (empty-stream)
    (stream-append (stream-fst ss) (delay (stream-flatten (stream-rst ss))))))

(define (stream-take n s)
  (if (or (stream-null? s) (and n (= n 0)))
    '()
    (cons (stream-fst s) (stream-take (and n (- n 1)) (stream-rst s)))))

;;; goal
;;; typeof(goal): assoc -> stream of assoc

(define *s (lambda (ass) (stream-of ass)))
(define *u (lambda (ass) (empty-stream)))

(define (stream-of-nullable e) (if e (stream-of e) (empty-stream)))

(define (== x y) (lambda (ass) (stream-of-nullable (unify ass x y))))

(define (join g1 g2)
  (lambda (ass)
    (let ([s1 (g1 ass)])
      (stream-flatten (stream-map (lambda (a) (g2 a)) s1)))))

(define-syntax conj
  (syntax-rules ()
    [(_ g) g]
    [(_ g1 g2) (join g1 g2)]
    [(_ g1 g2 gs ...) (conj (conj g1 g2) gs ...)]))

(define-syntax conde
  (syntax-rules ()
    [(_ [g gs ...]) (conj g gs ...)]
    [(_ [g gs ...] c* ...)
     (lambda (ass)
       (stream-append ((conj g gs ...) ass) (delay ((conde c* ...) ass))))]))

(define-syntax conda
  (syntax-rules ()
    [(_ [g gs ...]) (conj g gs ...)]
    [(_ [g gs ...] [g* gs* ...] ...)
     (lambda (ass)
       (let ([s (g ass)])
         ((if (stream-null? s)
            (conda [g* gs* ...] ...)
            (conj g gs ...))
           ass)))]))

(define-syntax condu
  (syntax-rules ()
    [(_ [g gs ...]) (conj g gs ...)]
    [(_ [g gs ...] [g* gs* ...] ...)
     (lambda (ass)
       (let ([s (g ass)])
         ((if (stream-null? s)
            (conda [g* gs* ...] ...)
            (conj gs ...))
           ass)))]))

(define-syntax let-fresh
  (syntax-rules ()
    [(_ (v) e es ...)
     (let ([v (var 'v)]) e es ...)]))

(define-syntax fresh
  (syntax-rules ()
    [(_ (v) g gs ...)
     (let-fresh (v) (conj g gs ...))]
    [(_ (v0 v1 vs ...) g gs ...)
     (fresh (v0)
       (fresh (v1 vs ...) g gs ...))]))

;;; run

(define-syntax run
  (syntax-rules ()
    [(_ n (q) g gs ...)
     (let-fresh (q)
       (let ([s ((conj g gs ...) (make-assoc))])
         (map reify (stream-take n (stream-map (lambda (ass) (walk q ass)) s)))))]
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
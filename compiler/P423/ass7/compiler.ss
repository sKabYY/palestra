; Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)
; Body -> (locals (uvar*) Tail)
; Tail -> Triv
;       | (binop Value Value)
;       | (Value Value*)
;       | (if Pred Tail Tail)
;       | (begin Effect* Tail)
; Pred -> (true)
;       | (false)
;       | (relop Value Value)
;       | (if Pred Pred Pred)
;       | (begin Effect* Pred)
; Effect -> (nop)
;         | (set! uvar Value)
;         | (Value Value*)
;         | (if Pred Effect Effect)
;         | (begin Effect* Effect)
; Value -> Triv
;        | (binop Value Value)
;        | (Value Value*)
;        | (if Pred Value Value)
;        | (begin Effect* Value)
; Triv -> uvar | int | label
; binop -> + | - | * | sra | logand | logor
; relop -> = | > | < | >= | <=
;

(case-sensitive #t)

(load "../lib/match.ss")
(load "../lib/helpers.ss")

;;;
; cont monad

(define-syntax do/k
  (syntax-rules (<-)
    [(_ e) e]
    [(_ (v* ... <- (rator rand* ...)) e e* ...)
     (rator rand* ... (lambda (v* ...)
                        (do/k e e* ...)))]
    [(_ e0 e e* ...)
     (begin e0 (do/k e e* ...))]))


;;;

; not used
(define (string-join sep lst)
  (if (null? lst)
      ""
      (apply string-append
             (car lst)
             (map (lambda (s) (string-append sep s))
                  (cdr lst)))))

(define (take n lst)
  (if (= n 0)
      '()
      (cons (car lst) (take (sub1 n) (cdr lst)))))

(define (skip-names name*)
  (unless (null? name*)
    (let ([max-count (apply max (map (lambda (l)
                                       (string->number (extract-suffix l)))
                                     name*))])
      (let loop ()
        (unless (>= (unique-name-count) max-count)
          (begin
            (unique-name 'skip)
            (loop)))))))

(define (binop? x)
  (memq x '(+ - * sra logand logor)))

(define (relop? x)
  (memq x '(= > < >= <=)))

(define (triv? v)
  (or (uvar? v) (number? v) (label? v)))

;;;

(define (verify-scheme pgm) pgm)


(define (uncover-target-conflict tail target?)
  (define conflicts '())
  (define (add-conflict var live*)
    (let ([ass (assq var conflicts)])
      (if ass
          (set-cdr! ass (union live* (cdr ass)))
          (set! conflicts (cons (cons var live*) conflicts)))))

  (define call-live* '())
  (define (add-call-live* live*)
    (set! call-live* (union call-live* live*)))

  (define (Triv triv)
    (if (or (uvar? triv) (target? triv)) `(,triv) '()))

  (define (uncover-set var live* triv-live*)
    (let ([new-live* (difference live* `(,var))])
      (cond
        [(uvar? var) (add-conflict var new-live*)]
        [(target? var)
         (let loop ([new-live* new-live*])
           (unless (null? new-live*)
             (when (uvar? (car new-live*))
               (add-conflict (car new-live*) `(,var)))
             (loop (cdr new-live*))))]
        [else (void)])
      (union new-live* triv-live*)))

  (define (live-analysis x live* false-live*)
    (define true-live* live*)
    (match x
      [(if ,prd ,[true-live*] ,[false-live*])
       (live-analysis prd true-live* false-live*)]
      [(begin) live*]
      [(begin ,effect* ... ,[lv*])
       (live-analysis `(begin ,effect* ...) lv* #f)]
      [(nop) live*]
      [(set! ,var (,binop ,[Triv -> triv1-live*] ,[Triv -> triv2-live*]))
       (uncover-set var live* (union triv1-live* triv2-live*))]
      [(set! ,var ,[Triv -> triv-live*])
       (uncover-set var live* triv-live*)]
      [(return-point ,rp-label ,[lv*]) (add-call-live* live*) lv*]
      [(true) true-live*]
      [(false) false-live*]
      [(,relop ,[Triv -> triv1-live*] ,[Triv -> triv2-live*])
       (guard (relop? relop))
       (union true-live* false-live* triv1-live* triv2-live*)]
      [(,[Triv -> triv-live*] ,loc* ...)
       (uncover-set return-value-register
                    live*
                    (union (filter target? loc*) triv-live*))]))

  (define (check-live-set live*)
    (let ([uvar* (filter uvar? live*)])
      (unless (null? uvar*)
        (error 'check-live-set "uninitialized var" uvar*))))

    (define (normalize conflicts)
      (let loop ([conflicts conflicts] [acc '()])
        (if (null? conflicts)
            (reverse acc)
            (let* ([conflict (car conflicts)]
                   [rst (cdr conflicts)]
                   [var (car conflict)])
              (let-values ([(vars locs) (partition (lambda (v)
                                                     (and (uvar? v) (assq v rst)))
                                                   (cdr conflict))])
                (for-each (lambda (v)
                            (add-conflict v (list var)))
                          vars)
                (loop rst (cons (cons var locs) acc)))))))

  (begin
    (check-live-set (live-analysis tail '() #f))
    (values (normalize conflicts) call-live*)))


(define (map-uil-body func pgm)
  (match pgm
    [(letrec ([,label* (lambda ,args* ,[func -> body*])] ...)
       ,[func -> body])
     `(letrec ([,label* (lambda ,args* ,body*)] ...) ,body)]))


(define (skip-used-name pgm)
  (match pgm
    [(letrec ([,label* (lambda ,args* ,body*)] ...) ,body)
     (skip-names label*)
     (skip-names (apply append args*))])
  pgm)


; Simple View:
; E -> Triv
;    | (E E*)
;    | (binop E E)
;    | (relop E E)
;    | (if E E E)
;    | (begin E* E)
;    | (set! uvar E)
;    | (nop)
;    | (true)
;    | (false)
;
; Flatten set!:
; F[X] -> (if P X X)
;       | (begin E* X)
; T -> Triv
;    | (binop Triv Triv)
;    | (Triv Triv*)
;    | F[T]
; E -> (nop)
;    | (set! uvar Triv)
;    | (set! uvar (binop Triv Triv))
;    | (return-point label (Triv Triv*))
;    | (set! uvar (return-point label (Triv Triv*)))
;    | F[E]
; P -> (true)
;    | (false)
;    | (relop Triv Triv)
;    | F[P]

(define (remove-complex-opera* pgm)

  (define (Tail tail)
    (define new-uvar* '())
    (define (new-temp-var)
      (let ([u (unique-name 't)])
        (set! new-uvar* (cons u new-uvar*))
        u))
    (define (id x) x)
    (define (return x) (lambda (ctx) (ctx x)))
    (define (set!-ctx uvar) (lambda (s) `(set! ,uvar ,s)))
    (define (with-u make-effect ctx)
      (let ([u (new-temp-var)])
        (make-begin (list (make-effect u) (ctx u)))))
    (define (rm ct)
      (lambda (x)
        (match x
          [(nop) (return `(nop))]
          [(true) (return `(true))]
          [(false) (return `(false))]
          [(set! ,uvar ,[(rm 'rhs) -> v])
           (return (v (set!-ctx uvar)))]
          [(begin ,[(rm 'seq) -> e*] ... ,[x0])
           (lambda (ctx)
             (make-begin `(,(map (lambda (e) (e id)) e*) ... ,(x0 ctx))))]
          [(if ,[(rm 'test) -> p] ,x1 ,x2)
           (lambda (ctx)
             (if (memq ct '(tail rhs seq test))
                 `(if ,(p id) ,(((rm ct) x1) ctx) ,(((rm ct) x2) ctx))
                 (with-u (lambda (u)
                           `(if ,(p id)
                                ,(((rm 'rhs) x1) (set!-ctx u))
                                ,(((rm 'rhs) x2) (set!-ctx u))))
                   ctx)))]
          [(,op ,[(rm 'app) -> v1] ,[(rm 'app) -> v2])
           (guard (or (binop? op) (relop? op)))
           (lambda (ctx)
             (do/k
              (s1 <- (v1))
              (s2 <- (v2))
              (let ([value `(,op ,s1 ,s2)])
                (if (memq ct '(tail rhs test))
                    (ctx value)
                    (with-u (lambda (u)
                              `(set! ,u ,value))
                      ctx)))))]
          [(,[(rm 'app) -> rator] ,[(rm 'app) -> rand*] ...)
           (lambda (ctx)
             (do/k
              (s0 <- (rator))
              (let loop ([r* rand*] [s* '()])
                (if (null? r*)
                    (let ([app `(,s0 ,(reverse s*) ...)])
                      (if (eq? ct 'tail)
                          (ctx app)
                          (let ([nontail `(return-point ,(unique-label 'rp)
                                                        ,app)])
                            (if (memq ct '(rhs seq))
                                (ctx nontail)
                                (with-u (lambda (u)
                                          `(set! ,u ,nontail))
                                  ctx)))))
                    (do/k
                     (s <- ((car r*)))
                     (loop (cdr r*) (cons s s*)))))))]
          [,triv
           (guard (or (uvar? triv) (number? triv) (label? triv)))
           (return triv)])))
    (values (reverse new-uvar*) (((rm 'tail) tail) id)))

  (define (Body body)
    (match body
      [(locals ,uvar* ,[Tail -> u* tail])
       `(locals (,uvar* ... ,u* ...) ,tail)]))

  (map-uil-body Body pgm))


(define (impose-calling-conventions pgm)
  (define parameter-registers '())

  (define (alloc-loc* n)
    (let loop ([n n] [regs parameter-registers] [idx 0] [acc '()])
      (if (= n 0)
          (reverse acc)
          (if (null? regs)
              (loop (sub1 n) '() (add1 idx) (cons (index->frame-var idx) acc))
              (loop (sub1 n) (cdr regs) idx (cons (car regs) acc))))))

  (define (alloc-nontail-param* n)
    (let loop ([n n] [regs parameter-registers] [acc '()])
      (if (= n 0)
          (reverse acc)
          (if (null? regs)
              (loop (sub1 n) '() (cons (unique-name 'nfv) acc))
              (loop (sub1 n) (cdr regs) (cons (car regs) acc))))))

  ; TODO: ?? before skip ??
  (define rp (unique-name 'rp))

  (define (make-ret v)
    `(begin (set! ,return-value-register ,v)
            (,rp ,frame-pointer-register ,return-value-register)))

  (define (Tail tail)
    (define new-frames '())
    (define (add-new-frame nfv*)
      (unless (null? nfv*)
        (set! new-frames (cons nfv* new-frames))))
    (define (impose x)
      (match x
        [(if ,[prd] ,[x1] ,[x2]) `(if ,prd ,x1 ,x2)]
        [(begin ,[x0]) x0]
        [(begin ,[effect*] ... ,[x0])
         (make-begin `(,effect* ... ,x0))]
        [(return-point ,rp-label (,rator ,rand* ...))
         (let ([param* (alloc-nontail-param* (length rand*))])
           (add-new-frame (filter uvar? param*))
           `(return-point ,rp-label
                          (begin
                            (set! ,param* ,rand*) ...
                            (set! ,return-address-register ,rp-label)
                            (,rator ,frame-pointer-register
                                    ,return-address-register
                                    ,param* ...))))]
        [(set! ,uvar (return-point ,rp-label ,app))
         `(begin ,(impose `(return-point ,rp-label ,app))
                 (set! ,uvar ,return-value-register))]
        [(set! ,uvar ,value) `(set! ,uvar ,value)]
        [(nop) '(nop)]
        [(true) '(true)]
        [(false) '(false)]
        [(,binop ,triv1 ,triv2) (guard (binop? binop))
         (make-ret `(,binop ,triv1 ,triv2))]
        [(,relop ,triv1 ,triv2) (guard (relop? relop))
         `(,relop ,triv1 ,triv2)]
        [(,rator ,rand* ...)
         (let ([loc* (alloc-loc* (length rand*))])
           `(begin
              (set! ,return-address-register ,rp)
              (set! ,loc* ,rand*) ...
              (,rator ,frame-pointer-register
                      ,return-address-register
                      ,loc* ...)))]
        [,triv (make-ret triv)]))
    (let ([tail (impose tail)])
      (values new-frames tail)))

  ;(display rp)
  (match pgm
    [(letrec ([,label* (lambda ,args*
                         (locals ,uvar**
                                 ,[Tail -> new-frames* tail*]))] ...)
       (locals ,u* ,[Tail -> new-frames tail]))
     `(letrec ([,label* (lambda ()
                          ,(map (lambda (uvar* args new-frames tail)
                                  `(locals
                                    (,uvar* ... ,rp ,args ... ,new-frames ...)
                                    (new-frames
                                     ,new-frames
                                     ; TODO: right loc*
                                     ,(let ([loc* (alloc-loc* (length args))])
                                        (make-begin
                                         `((set! ,rp ,return-address-register)
                                           (set! ,args ,loc*) ...
                                           ,tail))))))
                                uvar**
                                args*
                                new-frames*
                                tail*))] ...)
        (locals ,u*
                (new-frames ,new-frames
                            ,(make-begin
                              `((set! ,rp ,return-address-register)
                                ,tail)))))]))


(define (uncover-frame-conflict pgm)
  (map-uil-body
   (lambda (body)
     (match body
       [(locals
         ,uvar*
         (new-frames
          ,new-frame* ,tail))
        (let-values ([(fc-graph call-live*)
                      (uncover-target-conflict tail frame-var?)])
          `(locals
            ,uvar*
            (new-frames
             ,new-frame*
             (spills
              ,(filter uvar? call-live*)
              (frame-conflict
               ,fc-graph
               (call-live ,call-live* ,tail))))))]))
   pgm))


(define (pre-assign-frame pgm)

  ; the same as assign-registers
  (define (assign spill* fc-graph location*)
    (define (select-frame-var conflicts)
      (let loop ([idx 0])
        (let ([fvar (index->frame-var idx)])
          (if (memq fvar conflicts)
              (loop (add1 idx))
              fvar))))
    (let loop ([g fc-graph] [res (reverse location*)])
      (if (null? g)
          (reverse res)
          (let ([uvar (caar g)])
            (if (memq uvar spill*)
                (let ([fvar (select-frame-var
                             (map (lambda (v)
                                    (if (uvar? v)
                                        (let ([r (assq v res)])
                                          (and r (cadr r)))
                                        v))
                                  (cdar g)))])
                  (loop (cdr g) (cons (list uvar fvar) res)))
                (loop (cdr g) res))))))

  (map-uil-body
   (lambda (body)
     (match body
       [(locals
         ,uvar*
         (new-frames
          ,new-frame*
          (spills
           ,spill*
           (frame-conflict
            ,fc-graph
            (call-live ,call-live* ,tail)))))
        `(locals
          ,uvar*
          (new-frames
           ,new-frame*
           (locate
            ,(assign spill* fc-graph '())
            (frame-conflict
             ,fc-graph
             (call-live ,call-live* ,tail)))))]))
   pgm))


(define (assign-new-frame pgm)

  (define (frame-size call-live* location*)
    (let ([idx* (map (lambda (v)
                       (frame-var->index
                        (cond
                          [(uvar? v) (cadr (assq v location*))]
                          [(frame-var? v) v]
                          [error 'frame-size "not uvar or frame-var" v])))
                     call-live*)])
      (if (null? idx*)
          0
          (add1 (apply max idx*)))))

  (define (assign n)
    (lambda (new-frame)
      (let loop ([rst new-frame] [idx n] [res '()])
        (if (null? rst)
            (reverse res)
            (loop (cdr rst)
                  (add1 idx)
                  (cons (list (car rst) (index->frame-var idx))
                        res))))))

  (define (>> x n)
    (match x
      [(if ,[p] ,[x1] ,[x2])
       `(if ,p ,x1 ,x2)]
      [(begin ,[e*] ... ,[t])
       `(begin ,e* ... ,t)]
      [(return-point ,rp-label (begin ,e* ... ,app))
       (let ([nb (fxsll n align-shift)]
             [fp frame-pointer-register])
         `(begin (return-point ,rp-label
                               (begin
                                 ,e* ...
                                 (set! ,fp (+ ,fp ,nb))
                                 ,app))
                 (set! ,fp (- ,fp ,nb))))]
      [,x x]))

  (map-uil-body
   (lambda (body)
     (match body
       [(locals
         ,uvar
         (new-frames
          ,new-frame*
          (locate
           ,location*
           (frame-conflict
            ,fc-graph
            (call-live ,call-live* ,tail)))))
        (let ([n (frame-size call-live* location*)])
          `(locals
            ,uvar
            (ulocals
             ()
             (locate
              (,(map (assign n) new-frame*) ... ... ,location* ...)
              (frame-conflict ,fc-graph ,(>> tail n))))))]))
   pgm))


; TODO: use do/k
(define (select-instructions pgm)

  (define (select/k x unspill* cont)
    ; TODO: switch opands
    (define (cont-u trunk)
      (let ([u (unique-name 'u)])
        (cont (trunk u) (cons u unspill*))))
    (define (>>) (cont x unspill*))
    (match x
      [(if ,prd ,x1 ,x2)
       (do/k
        (prd unspill* <- (select/k prd unspill*))
        (x1 unspill* <- (select/k x1 unspill*))
        (x2 unspill* <- (select/k x2 unspill*))
        (cont `(if ,prd ,x1 ,x2) unspill*))]
      [(begin ,x0) (select/k x0 unspill* cont)]
      [(begin ,ef ,ef* ... ,x0)
       (select/k ef unspill*
                 (lambda (ef unspill*)
                   (select/k `(begin ,ef* ... ,x0) unspill*
                             (lambda (ef* unspill*)
                               (cont (make-begin (list ef ef*))
                                     unspill*)))))]
      [(nop) (>>)]
      [(set! ,var (,binop ,triv1 ,triv2))
       (if (frame-var? var)
           (if (eq? var triv1)
               (if (frame-var? triv2)
                   (cont-u (lambda (u)
                             `(begin (set! ,u ,triv2)
                                     (set! ,var (,binop ,var ,u)))))
                   (>>))
               (cont-u (lambda (u)
                         `(begin (set! ,u ,triv1)
                                 (set! ,u (,binop ,u ,triv2))
                                 (set! ,var ,u)))))
           (if (eq? var triv1)
               (>>)
               (cont-u (lambda (u)
                         `(begin (set! ,u ,triv1)
                                 (set! ,u (,binop ,u ,triv2))
                                 (set! ,var ,u))))))]
      [(set! ,var ,triv)
       (if (and (frame-var? var)
                (not (number? triv))
                (not (register? triv))
                (not (uvar? triv)))
           (cont-u (lambda (u)
                     `(begin (set! ,u ,triv)
                             (set! ,var ,u))))
           (>>))]
      [(return-point ,rp-label ,tail)
       (do/k
        (tail unspill* <- (select/k tail unspill*))
        (cont `(return-point ,rp-label ,tail) unspill*))]
      [(true) (>>)]
      [(false) (>>)]
      [(,relop ,triv1 ,triv2) (guard (relop? relop))
       (if (or (number? triv1)
               (and (frame-var? triv1) (frame-var? triv2)))
           (cont-u (lambda (u)
                     `(begin (set! ,u ,triv1)
                             (,relop ,u ,triv2))))
           (>>))]
      [(,triv ,loc* ...) (>>)]))

  (define (Body body)
    (match body
      [(locals ,uvar*
        (ulocals ,unspill*
          (locate ,location*
           (frame-conflict ,fc-graph ,tail))))
       (select/k tail unspill* (lambda (tail unspill*)
                                 `(locals
                                   ,uvar*
                                   (ulocals
                                    ,unspill*
                                    (locate
                                     ,location*
                                     (frame-conflict ,fc-graph ,tail))))))]
      [,body body]))

  (map-uil-body Body pgm))


(define (uncover-register-conflict pgm)
  (map-uil-body (lambda (body)
              (match body
                [(locals
                  ,uvar*
                  (ulocals
                   ,unspill*
                   (locate
                    ,location*
                    (frame-conflict ,fc-graph ,tail))))
                 (let-values ([(rc-graph call-live*)
                               (uncover-target-conflict tail register?)])
                   `(locals
                     ,uvar*
                     (ulocals
                      ,unspill*
                      (locate
                       ,location*
                       (frame-conflict
                        ,fc-graph
                        (register-conflict ,rc-graph ,tail))))))]
                [,body body]))
            pgm))


(define (assign-registers pgm)
  ;(define registers '(rax rbp r8 r9))
  (define registers '(rax r8))

  ; very simple assign (linear assign)
  (define (assign  unspill* rc-graph)
    (define (select-register regs conflicts)
      (if (null? regs)
          #f
          (if (memq (car regs) conflicts)
              (select-register (cdr regs) conflicts)
              (car regs))))
    (define (select-spills uvar* unspill* n)
      (let ([spillable* (filter (lambda (v) (not (memq v unspill*)))
                                (reverse uvar*))])
        (if (> n (length spillable*))
            (error 'assign-register "not enough registers")
            (reverse (take n spillable*)))))
    (let loop ([g rc-graph] [res '()])
      (if (null? g)
          (values (reverse res) '())
          (let ([uvar (caar g)]
                [reg (select-register registers
                                      (map (lambda (v)
                                             (if (uvar? v)
                                                 (cadr (assq v res))
                                                 v))
                                           (cdar g)))])
            (if reg
                (loop (cdr g) (cons (list uvar reg) res))
                (values #f (select-spills (map car rc-graph)
                                          unspill*
                                          (length g))))))))

  (define (Body body)
    (match body
      [(locals
        ,uvar*
        (ulocals
         ,unspill*
         (locate
          ,location*
          (frame-conflict
           ,fc-graph
           (register-conflict ,rc-graph ,tail)))))
       (let-values ([(ret spill*) (assign unspill* rc-graph)])
         (if ret
             `(locate ,ret ,tail)
             `(locals
               ,uvar*
               (ulocals
                ,unspill*
                (spills
                 ,spill*
                 (locate
                  ,location*
                  (frame-conflict ,fc-graph ,tail)))))))]
      [,body body]))

  (map-uil-body Body pgm))


(define (everybody-home? pgm)
  ; todo: more beautiful implementation
  (define ret #t)
  (define (Body body)
    (set! ret (and ret (eq? (car body) 'locate)))
    body)
  (map-uil-body Body pgm)
  ret)


(define (assign-frame pgm)

  ; the same as assign-registers
  (define (assign spill* fc-graph location*)
    (define (select-frame-var conflicts)
      (let loop ([idx 0])
        (let ([fvar (index->frame-var idx)])
          (if (memq fvar conflicts)
              (loop (add1 idx))
              fvar))))
    (let loop ([g fc-graph] [res (reverse location*)])
      (if (null? g)
          (reverse res)
          (let ([uvar (caar g)])
            (if (memq uvar spill*)
                (let ([fvar (select-frame-var
                             (map (lambda (v)
                                    (if (uvar? v)
                                        (let ([r (assq v res)])
                                          (and r (cadr r)))
                                        v))
                                  (cdar g)))])
                  (loop (cdr g) (cons (list uvar fvar) res)))
                (loop (cdr g) res))))))

  (define (Body body)
    (match body
      [(locals
        ,uvar*
        (ulocals
         ,unspill*
         (spills
          ,spill*
          (locate
           ,location*
           (frame-conflict ,fc-graph ,tail)))))
       `(locals
         ,uvar*
         (ulocals
          ,unspill*
          (locate
           ,(assign spill* fc-graph location*)
           (frame-conflict ,fc-graph ,tail))))]
      [,body body]))

  (map-uil-body Body pgm))


(define (finalize-target x env target?)
  (define (lookup v env)
    (let ([slot (assq v env)])
      (if (and slot (target? (cdr slot)))
          (cdr slot)
          v)))
  (match x
    [(if ,[p] ,[e1] ,[e2]) `(if ,p ,e1 ,e2)]
    [(begin ,[ef*] ... ,[tail]) `(begin ,ef* ... ,tail)]
    [(set! ,[var] (,binop ,[triv1] ,[triv2]))
     `(set! ,var (,binop ,triv1 ,triv2))]
    [(set! ,[var] ,[triv])
     (if (eq? var triv)
         `(nop)
         `(set! ,var ,triv))]
    [(return-point ,rp-label ,[tail])
     `(return-point ,rp-label ,tail)]
    [(,relop ,[triv1] ,[triv2]) (guard (relop? relop))
     `(,relop ,triv1 ,triv2)]
    [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
    [,v (guard (uvar? v)) (lookup v env)]
    [,x x]))


(define (finalize-frame-locations pgm)
  (define (Body body)
    (match body
      [(locals
        ,uvar*
        (ulocals
         ,unspill*
         (locate
          ([,uv* ,v*] ...)
          (frame-conflict ,fc-graph ,tail))))
       `(locals
         ,uvar*
         (ulocals
          ,unspill*
          (locate
           ([,uv* ,v*] ...)
           (frame-conflict
            ,fc-graph
            ,(finalize-target tail `((,uv* . ,v*) ...) frame-var?)))))]
      [,body body]))
  (map-uil-body Body pgm))


(define (discard-call-live pgm)

  (define (discard x)
    (match x
      [(if ,[p] ,[x1] ,[x2]) `(if ,p ,x1 ,x2)]
      [(begin ,[e*] ... ,[x0]) `(begin ,e* ... ,x0)]
      [(return-point ,rp-label ,[tail])
       `(return-point ,rp-label ,tail)]
      [(set! ,a ,b) `(set! ,a ,b)]
      [(nop) `(nop)]
      [(,relop ,triv1 ,triv2) (guard (relop? relop))
       `(,relop ,triv1 ,triv2)]
      [(,triv ,loc* ...) `(,triv)]))

  (define (Body body)
    (match body
      [(locate ,locates ,[discard -> tail])
       `(locate ,locates ,tail)]))

  (match pgm
    [(letrec ([,label* (lambda () ,[Body -> body*])] ...)
       ,[Body -> body])
     `(letrec ([,label* (lambda () ,body*)] ...)
        ,body)]))


(define (finalize-locations pgm)

  (define (Body body)
    (match body
      [(locate ([,uvar* ,loc*] ...) ,tail)
       (finalize-target tail `((,uvar* . ,loc*) ...) register?)]))

  (define (Dec dec)
    (match dec
      [(,label (lambda () ,[Body -> body]))
       `(,label (lambda () ,body))]))

  (match pgm
    [(letrec (,[Dec -> dec*] ...) ,[Body -> body])
     `(letrec ,dec* ,body)]))


(define (expose-frame-var pgm)

  (define (Tail tail)
    (match tail
      [(if ,[Pred -> prd] ,[tail1] ,[tail2])
       `(if ,prd ,tail1 ,tail2)]
      [(begin ,[Effect -> effect*] ... ,[tl])
       `(begin ,effect* ... ,tl)]
      [(,[Triv -> triv]) `(,triv)]))

  (define (Pred prd)
    (match prd
      [(if ,[pd0] ,[pd1] ,[pd2])
       `(if ,pd0 ,pd1 ,pd2)]
      [(begin ,[Effect -> effect*] ... ,[pd])
       `(begin ,effect* ... ,pd)]
      [(,relop ,[Triv -> triv1] ,[Triv -> triv2])
       `(,relop ,triv1 ,triv2)]
      [,x x]))

  (define (Effect effect)
    (match effect
      [(set! ,[Var -> var] (,binop ,[Triv -> triv1] ,[Triv -> triv2]))
       `(set! ,var (,binop ,triv1 ,triv2))]
      [(set! ,[Var -> var] ,[Triv -> triv])
       `(set! ,var ,triv)]
      [(return-point ,rp-label ,[Tail -> tail])
       `(return-point ,rp-label ,tail)]
      [(if ,[Pred -> prd] ,[effect1] ,[effect2])
       `(if ,prd ,effect1 ,effect2)]
      [(begin ,[effect*] ... ,[effect])
       `(begin ,effect* ... ,effect)]
      [(nop) `(nop)]))

  (define (Triv triv)  ; TODO: label
    (if (number? triv)
        triv
        (Var triv)))

  ; TODO: BYTES-OF-WORD => align-shift
  (define (Var var)
    (cond
      [(frame-var->index var)
       => (lambda (idx) (make-disp-opnd frame-pointer-register (* idx BYTES-OF-WORD)))]
      [else var]))

  (define (Dec dec)
    (match dec
      [(,label (lambda () ,[Tail -> tail]))
       `(,label (lambda () ,tail))]))

  (match pgm
    [(letrec (,[Dec -> dec*] ...) ,[Tail -> tail])
     `(letrec ,dec* ,tail)]))


(define (expose-basic-blocks pgm)

  (define new-dec* '())
  (define (add-dec! l tail)
    (let ([label (unique-label l)])
      (set! new-dec* (cons `[,label (lambda () ,tail)] new-dec*))
      label))

  (define (jump? e)
    (match e
      [(,label) (label? label)]
      [,e #f]))

  (define (make-jump l tail)
    (if (jump? tail) tail `(,(add-dec! l tail))))

  (define (make-c-jump tail) (make-jump 'c tail))
  (define (make-a-jump tail) (make-jump 'a tail))
  (define (make-j-jump tail) (make-jump 'j tail))

  (define (begin-concat e t)
    (match t
      [(begin ,tl* ...) `(begin ,e ,tl* ...)]
      [,t `(begin ,e ,t)]))

  (define (expose-seq e* x >>)
    (if (null? e*)
        (>> x)
        (let ([ef (Effect (car e*))]
              [tail (expose-seq (cdr e*) x >>)])
          (if (procedure? tail)
              (lambda tl* (ef (apply tail tl*)))
              (ef tail)))))

  (define (Tail tail)
    (match tail
      [(if ,[Pred -> prd] ,[tail1] ,[tail2])
       (prd tail1 tail2)]
      [(begin ,effect* ... ,tl)
       (expose-seq effect* tl Tail)]
      [,tail tail]))

  (define (Pred prd)
    (match prd
      [(if ,[prd0] ,[prd1] ,[prd2])
       (lambda (tail1 tail2)
         (let ([c-jmp (make-c-jump tail1)]
               [a-jmp (make-a-jump tail2)])
           (let ([if-tail1 (prd1 c-jmp a-jmp)]
                 [if-tail2 (prd2 c-jmp a-jmp)])
             (prd0 if-tail1 if-tail2))))]
      [(begin ,effect* ... ,prd)
       (expose-seq effect* prd Pred)]
      [(true) (lambda (tail1 tail2) tail1)]
      [(false) (lambda (tail1 tail2) tail2)]
      [,prd (lambda (tail1 tail2)
              `(if ,prd ,(make-c-jump tail1) ,(make-a-jump tail2)))]))

  (define (Effect effect)
    (match effect
      [(if ,[Pred -> prd] ,[Effect -> effect1] ,[Effect -> effect2])
       (lambda (tail)
         (let ([jmp (make-j-jump tail)])
           (prd (effect1 jmp) (effect2 jmp))))]
      [(begin ,effect* ... ,ef)
       (expose-seq effect* ef Effect)]
      [(return-point ,rp-label ,[Tail -> tl])
       (lambda (tail) (make-jump rp-label tail) tl)]
      [(nop) (lambda (tail) tail)]
      [,effect (lambda (tail) (begin-concat effect tail))]))

  (match pgm
    [(letrec ([,label* (lambda () ,[Tail -> tl*])] ...) ,[Tail -> tail])
     `(letrec ([,label* (lambda () ,tl*)] ... ,new-dec* ...) ,tail)]))


(define (flatten-program pgm)

  (define (Tail tail next-label)
    (match tail
      [(begin ,effect* ... ,[tl])
       `(,@effect* ,@tl)]
      [(if ,prd (,label1) (,label2))
       (cond
         [(eq? label2 next-label)
          `((if ,prd (jump ,label1)))]
         [(eq? label1 next-label)
          `((if (not ,prd) (jump ,label2)))]
         [else
          `((if ,prd (jump ,label1))
            (jump ,label2))])]
      [(,triv)
       (if (eq? triv next-label) '() `((jump ,triv)))]))

  (define (Dec dec next-label)
    (match dec
      [(,label (lambda () ,tail))
       (cons label (Tail tail next-label))]))

  (define (flatten-dec* dec* next-label)
    ; assert (not (null? dec*))
    (if (null? (cdr dec*))
        (list (Dec (car dec*) next-label))
        (let ([rst (flatten-dec* (cdr dec*) next-label)])
          (cons (Dec (car dec*) (caar rst)) rst))))

  (match pgm
    [(letrec ,dec* ,tail)
     `(code ,@(if (null? dec*)
                  (Tail tail #f)
                  (let ([decs (flatten-dec* dec* #f)])
                    `(,(Tail tail (caar decs)) ...
                      ,decs ... ...))))]))


; generator-x86-64

;[regs '(rax rcx rdx rbx rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)]

(define BYTES-OF-WORD 8)

(define (generate-x86-64 pgm)

  (define (stm<= stm)
    (define binop-map '([+ . addq]
                        [- . subq]
                        [* . imulq]
                        [sra . sarq]
                        [logand . andq]
                        [logor . orq]))
    (define relop-map '([= . (je . jne)]
                        [> . (jg . jle)]
                        [>= . (jge . jl)]
                        [< . (jl . jge)]
                        [<= . (jle . jg)]))
    (let ([binop-lookup (lambda (op)
                          (cond
                            [(assq op binop-map) => cdr]
                            [else (error 'binop-lookup
                                         "unsupported binop" op)]))]
          [relop-lookup (lambda (op pos?)
                          (cond
                            [(assq op relop-map) => (if pos? cadr cddr)]
                            [else (error 'relop-lookup
                                         "unsupported relop" op)]))])
      (match stm
        [(set! ,var (,binop ,var ,triv))
         (emit (binop-lookup binop) triv var)]
        [(set! ,var ,triv)
         (if (label? triv)
             (emit 'leaq triv var)
             (emit 'movq triv var))]
        [(if (,relop ,triv1 ,triv2) (jump ,label))
         (emit 'cmpq triv2 triv1)
         (emit-jump (relop-lookup relop #t) label)]
        [(if (not (,relop ,triv1 ,triv2)) (jump ,label))
         (emit 'cmpq triv2 triv1)
         (emit-jump (relop-lookup relop #f) label)]
        [(jump ,triv) (emit-jump 'jmp triv)]
        [,label (guard (label? label)) (emit-label label)])))

  (match pgm
    [(code ,stm+ ...)
     (emit-program (for-each (lambda (stm) (stm<= stm)) stm+))]))


(define (compiler-passes . passes)
  (lambda (pgm)
    (fold-left (lambda (acc pass)
                 (pass acc))
               pgm
               passes)))

(define (iterate . passes)
  (define (this-pass pgm)
    (let loop ([ps passes] [pgm pgm])
      (if (null? ps)
          (this-pass pgm)
          (let ([ret ((car ps) pgm)])
            (if ret
                (loop (cdr ps) ret)
                pgm)))))
  this-pass)

(define (break-if pred?)
  (lambda (pgm) (and (not (pred? pgm)) pgm)))

(define (probe pass)
  (lambda (pgm)
    (let ([ret (pass pgm)])
      (if (eq? ret (void))
          (void)
          (pretty-print ret))
      ret)))

(define compiler
  (compiler-passes
   skip-used-name
   remove-complex-opera*
   impose-calling-conventions
   uncover-frame-conflict
   pre-assign-frame
   assign-new-frame

   (iterate
    ;(break-if (lambda (pgm) #t))
    finalize-frame-locations
    select-instructions
    uncover-register-conflict
    assign-registers
    (break-if everybody-home?)
    assign-frame
    )

   discard-call-live
   finalize-locations
   expose-frame-var
   expose-basic-blocks
   flatten-program
   generate-x86-64
   ))

(define test1
  '(letrec ([f$1 (lambda (a.11 b.2 d.4)
                   (locals (c.3)
                           (begin
                             (set! c.3 (+ a.11
                                          (+ a.11
                                             (if (> a.11 b.2)
                                                 (+ a.11 a.11)
                                                 (+ b.2 b.2)))))
                             (if (if (true)
                                     (> c.3 (+ a.11 a.11))
                                     (+ 1 (begin
                                            (set! a.11 1)
                                            (+ 1 1))))
                                 (set! b.2 99)
                                 (nop))
                             (set! b.2 (begin
                                         (set! b.2 (+ b.2 c.3))
                                         (set! a.11 1)
                                         (+ (+ a.11 a.11) b.2)))
                             (+ b.2 d.4))))])
     (locals () (f$1 1 2 3))))

(define test2 '(letrec ([f$1 (lambda ()
                               (locals ()
                                       1))])
                 (locals () (f$1))))

(define test3 '(letrec ()
                 (locals
                  (a.1)
                  (begin
                    (set! a.1 (+ 1 (if (true) 2 3)))
                    a.1))))

(define test4 '(letrec ()
                 (locals (a.1 b.2 c.3 d.4)
                         (begin
                           (set! a.1 99)
                           (set! b.2 88)
                           (if (true)
                               (set! c.3 a.1)
                               (set! c.3 b.2))
                           (set! d.4 11)
                           (set! d.4 (+ d.4 c.3))
                           d.4))))

(define test5 '(letrec ([f$1 (lambda (a.1 b.2 c.3)
                               (locals
                                ()
                                (begin
                                  (if (begin (set! a.1 (+ b.2 a.1))
                                             (> a.1 b.2))
                                      (set! a.1 (begin
                                                  (set! a.1 1)
                                                  (+ a.1 b.2)))
                                      (set! a.1 (if (true) 1 2)))
                                  (+ a.1 b.2))))]
                        [g$2 (lambda (a.1 b.2 c.3)
                               (locals () (+ a.1
                                             (begin
                                               (set! a.1 2)
                                               (+ a.1 1)))))])
                 (locals (c.3)
                         (begin
                           (set! c.3 (+ 1 (if (true) 3 4)))
                           (g$2 1 2 3)
                           (f$1 1
                                (if (true)
                                    (if (true) 2 3)
                                    (if (true) 1 4))
                                (g$2 2 3 4))))))

; TODO: the order of set! and app
(define test6 '(letrec () (locals (a.1)
                                  (begin
                                    (set! a.1 1)
                                    (+ a.1 (begin
                                             (set! a.1 2)
                                             a.1))))))

; test call-live
(define test7 '(letrec ([f$1 (lambda (a.1 b.2)
                               (locals () (+ a.1 b.2)))])
                 (locals (a.1 b.2)
                         (begin
                           (set! a.1 1)
                           (set! b.2 2)
                           (set! a.1 (f$1 a.1 b.2))
                           (f$1 a.1 2)))))

(define test-gcd '(letrec ([r$1 (lambda (a.1 b.2)
                                  (locals
                                   ()
                                   (if (< a.1 b.2)
                                       a.1
                                       (r$1 (- a.1 b.2) b.2))))]
                           [gcd$2 (lambda (a.1 b.2)
                                    (locals
                                     ()
                                     (if (= b.2 0)
                                         a.1
                                         (gcd$2 b.2 (r$1 a.1 b.2)))))])
                    (locals () (gcd$2 144 12144))))

(let ([pgm test6])
  (printf "source: ~n")
  (pretty-print pgm)
  (printf "~n~n")
  (let ([ret (with-output-to-file "_local_t.s"
               (lambda () (compiler pgm))
               'truncate)])
    (if (eq? ret (void))
        (void)
        (pretty-print ret))))

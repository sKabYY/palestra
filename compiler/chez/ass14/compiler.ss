; Program -> Expr
; Expr -> uvar
;       | (quote datum)
;       | (if Expr Expr Expr)
;       | (begin Expr* Expr)
;       | (lambda (uvar*) Expr)
;       | (let ([uvar Expr]*) Expr)
;       | (letrec ([uvar Expr]*) Expr)
;       | (set! uvar Expr)
;       | (prim Expr*)
;       | (Expr Expr*)
;
; Immediate -> fixnum | () | #t | #f
; datum -> Immediate | pair | vector
; prim -> value-prim | pred-prim | effect-prim
; value-prim -> + | - | * | car | cdr | cons | make-vector | vector-length | vector-ref | void
; pred-prim -> <= | < | = | >= | > | boolean? | eq? | fixnum? | null? | pair? | vector?
; effect-prim -> set-car! | set-cdr! | vector-set!
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;UIL:
; Program -> (letrec ([label (lambda (uvar*) Body)]*) Body)
; Body -> (locals (uvar*) Tail)
; Tail -> Triv
;       | (alloc Value)
;       | (mref Value Value)
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
;         | (mset! Value Value Value)
;         | (Value Value*)
;         | (if Pred Effect Effect)
;         | (begin Effect* Effect)
; Value -> Triv
;        | (alloc Value)
;        | (mref Value Value)
;        | (binop Value Value)
;        | (Value Value*)
;        | (if Pred Value Value)
;        | (begin Effect* Value)
;
; Triv -> uvar | int | label
; binop -> + | - | * | sra | logand | logor
; relop -> = | > | < | >= | <=
;

(case-sensitive #t)

(load "../lib/match.ss")
(load "../lib/helpers.ss")
(load "../lib/fmts.pretty")
(load "../lib/driver.ss")

(define parameter-registers '())
(define registers '(rax rbx rcx rdx rbp r8 r9))
;(define registers '(rax r8))

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

(define-syntax letv*
  (syntax-rules ()
    [(_ () body ...) (begin body ...)]
    [(_ ([(x0 ...) v0] [x1 v1] ...) body ...)
     (let-values ([(x0 ...) v0])
       (letv* ([x1 v1] ...) body ...))]
    [(_ ([x0 v0] [x1 v1] ...) body ...)
     (let ([x0 v0])
       (letv* ([x1 v1] ...) body ...))]))

(define (id x) x)

(define (extract-prefix u) (string->symbol (extract-root u)))

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

(define (mref? v)
  (and (pair? v) (eq? (car v) 'mref)))

(define (mem? v)
  (or (frame-var? v) (mref? v)))

(define (value-prim? x)
  (memq x '(+ - * car cdr cons make-vector vector-length vector-ref void)))

(define (pred-prim? x)
  (memq x '(<= < = >= > boolean? eq? fixnum? null? pair? vector? procedure?)))

(define (effect-prim? x)
  (memq x '(set-car! set-cdr! vector-set!)))

(define (prim? x) (or (value-prim? x) (pred-prim? x) (effect-prim? x)))

(define (make-let dec* body)
  (if (null? dec*)
      body
      `(let ,dec* ,body)))

(define (make-letrec dec* body)
  (if (null? dec*)
      body
      `(letrec ,dec* ,body)))

;;;


(define (skip-used-name pgm)
  (define (find-names x)
    (match x
      [(,[n*] ...) `(,n* ... ...)]
      [,n (guard (or (uvar? n) (label? n))) (list n)]
      [,x '()]))
  (skip-names (find-names pgm))
  pgm)


(define (convert-complex-datum pgm)

  (define (convert-const datum)
    (match datum
      [() ''()]
      [(,[a] . ,[d]) `(cons ,a ,d)]
      [#(,[e*] ...)
       (let ([n (length e*)]
             [tmp (unique-name 'tmp-vec)])
         `(let ([,tmp (make-vector (quote ,n))])
            (begin
              ,@(let loop ([e* e*] [idx 0])
                  (if (null? e*)
                      '()
                      (cons
                       `(vector-set! ,tmp ',idx ,(car e*))
                       (loop (cdr e*) (add1 idx)))))
              ,tmp)))]
      [,imm
       (guard (or (boolean? imm) (number? imm)))
       `(quote ,imm)]
      [,x (error 'convert-complex-datum "invalid const" x)]))

  (define (convert x)
    (match x
      [(quote ,datum) (convert-const datum)]
      [(,[h] ,[t*] ...) `(,h ,t* ...)]
      [,x x]))

  (convert pgm))


(define (uncover-assigned pgm)

  (define (uncover x)
    (match x
      [(let ([,u* ,[e* as*]] ...) ,[body as])
       (let-values ([(as^ free^) (partition (lambda (v) (memq v u*)) as)])
         (values
          `(let ([,u* ,e*] ...)
             (assigned ,as^ ,body))
          (apply union free^ as*)))]
      [(lambda (,a* ...) ,[body as])
       (let-values ([(as^ free^) (partition (lambda (v) (memq v a*)) as)])
         (values
          `(lambda (,a* ...) (assigned ,as^ ,body))
          free^))]
      [(letrec ([,u* ,[e* as*]] ...) ,[body as])
       (let-values ([(as^ free^)
                     (partition (lambda (v) (memq v u*)) (apply union as as*))])
         (values
          `(letrec ([,u* ,e*] ...)
             (assigned ,as^ ,body))
          free^))]
      [(set! ,u ,[e as])
       (values `(set! ,u ,e) (union `(,u) as))]
      [(,[h as] ,[t* as*] ...)
       (values `(,h ,t* ...) (apply union as as*))]
      [,x (values x '())]))

  (let-values ([(pgm^ assigned) (uncover pgm)]) pgm^))


(define (purify-letrec pgm)

  (define (tmp-vars u*)
    (map (lambda (u) (unique-name (extract-prefix u))) u*))

  ; TODO
  (define (purify0 x)
    (match x
      [(,let/rec () (assigned () ,[body]))
       (guard (memq let/rec '(let letrec)))
       body]
      [(letrec ([,u* ,[e*]] ...) (assigned (,as ...) ,[e]))
       (let ([t* (tmp-vars u*)])
         `(let ([,u* (void)] ...)
            (assigned (,u* ...)
                      (begin
                        (let ([,t* ,e*] ...)
                          (assigned ()
                                    (begin
                                      (set! ,u* ,t*) ...)))
                        ,e))))]
      [(,[h] ,[t*] ...) `(,h ,t* ...)]
      [,x x]))

  (purify0 pgm))


(define (convert-assignments pgm)

  (define (tmp-vars u*) (map (lambda (u) (unique-name 't)) u*))

  (define (convert x env)
    (match x
      [(let ([,u* ,[e*]] ...) (assigned (,as ...) ,e))
       (let loop ([u* u*] [u^* '()] [d* '()])
         (if (null? u*)
             (let ([u^* (reverse u^*)]
                   [d* (reverse d*)])
               `(let ([,u^* ,e*] ...)
                  ,(make-let d* (convert e (append env as)))))
             (let ([u (car u*)])
               (if (memq u as)
                   (let ([tmp (unique-name 't)])
                     (loop (cdr u*)
                           (cons tmp u^*)
                           (cons `(,u (cons ,tmp (void))) d*)))
                   (loop (cdr u*) (cons u u^*) d*)))))]
      [(lambda (,a* ...) (assigned (,as ...) ,e))
       (let loop ([a* a*] [a^* '()] [d* '()])
         (if (null? a*)
             (let ([a^* (reverse a^*)]
                   [d* (reverse d*)])
               `(lambda (,a^* ...)
                  ,(make-let d* (convert e (append env as)))))
             (let ([a (car a*)])
               (if (memq a as)
                   (let ([tmp (unique-name 't)])
                     (loop (cdr a*)
                           (cons tmp a^*)
                           (cons `(,a (cons ,tmp (void))) d*)))
                   (loop (cdr a*) (cons a a^*) d*)))))]
      [(set! ,u ,[e])
       (if (memq u env)
           `(set-car! ,u ,e)
           (error 'convert-assignments "not assigned variable!" u))]
      [(,[e*] ...) `(,e* ...)]
      [,u (guard (memq u env)) `(car ,u)]
      [,x x]))

  (convert pgm '()))


(define (remove-anonymous-lambda pgm)

  (define (rm x)
    (match x
      [((lambda (,p* ...) ,e) ,a* ...)
       (rm `(let ([,p* ,a*] ...) ,e))]
      [(letrec ([,uvar* (lambda (,a** ...) ,[e*])] ...) ,[e])
       `(letrec ([,uvar* (lambda (,a** ...) ,e*)] ...) ,e)]
      [(let ([,v* ,e*] ...) ,[e])
       (let loop ([v* v*] [e* e*] [let-d* '()] [letrec-d* '()])
         (if (null? v*)
             (make-let (reverse let-d*)
                       (make-letrec (reverse letrec-d*) e))
             (match (car e*)
               [(lambda (,a* ...) ,b)
                (loop (cdr v*)
                      (cdr e*)
                      let-d*
                      (cons `(,(car v*) (lambda (,a* ...) ,(rm b)))
                            letrec-d*))]
               [,e0
                (loop (cdr v*)
                      (cdr e*)
                      (cons `(,(car v*) ,(rm e0)) let-d*)
                      letrec-d*)])))]
      [(lambda (,a* ...) ,[e])
       (let ([f (unique-name 'anonymous-func)])
         `(letrec ([,f (lambda (,a* ...) ,e)]) ,f))]
      [(,[h] ,[t*] ...) `(,h ,t* ...)]
      [,x x]))

  (rm pgm))


(define (uncover-free pgm)

  (define (uncover x)
    (match x
      [(letrec ([,f* (lambda (,a** ...) ,[e* free*])] ...) ,[e free])
       (let ([free^* (map difference free* a**)])
         (values
          `(letrec ([,f* (lambda (,a** ...) (free ,free^* ,e*))] ...)
             ,e)
          (difference (union (apply union free^*) free) f*)))]
      [(if ,[p frp] ,[x1 fr1] ,[x2 fr2])
       (values `(if ,p ,x1 ,x2) (union frp fr1 fr2))]
      [(begin ,[e* fre*] ... ,[x0 fr0])
       (values `(begin ,e* ... ,x0) (apply union fr0 fre*))]
      [(let ([,v* ,[e* free*]] ...) ,[e free])
       (values
        `(let ([,v* ,e*] ...) ,e)
        (difference (apply union free free*) v*))]
      [(quote ,imm) (values `(quote ,imm) '())]
      [(,prim ,[e* fr*] ...) (guard (prim? prim))
       (values `(,prim ,e* ...) (apply union fr*))]
      [(,[rator fr] ,[rand* fr*] ...)
       (values `(,rator ,rand* ...) (apply union fr fr*))]
      [,uvar (guard (uvar? uvar)) (values uvar (list uvar))]
      [,x (error 'uncover-free "invalid expression" x)]))

  (let-values ([(pgm^ free) (uncover pgm)]) pgm^))


(define (convert-closures pgm)

  (define (unique-names sym n)
    (if (= n 0)
        '()
        (cons (unique-name sym) (unique-names sym (sub1 n)))))

  (define (convert x)
    (match x
      [(letrec ([,uvar* (lambda (,a** ...) (free (,free* ...) ,[e*]))] ...)
         ,[e])
       (let ([n (length uvar*)])
         (let ([cp* (unique-names 'cp n)]
               [label* (map (lambda (uvar)
                              (unique-label (extract-prefix uvar)))
                            uvar*)])
           `(letrec ([,label* (lambda (,cp* ,a** ...)
                                (bind-free (,cp* ,free* ...) ,e*))] ...)
              (closures ([,uvar* ,label* ,free* ...] ...) ,e))))]
      [(let ([,uvar* ,[e*]] ...) ,[e])
       `(let ([,uvar* ,e*] ...) ,e)]
      [(,h ,[t*] ...) (guard (or (memq h '(if begin quote)) (prim? h)))
       `(,h ,t* ...)]
      [(,[rator] ,[rand*] ...)
       (if (uvar? rator)
           `(,rator ,rator ,rand* ...)
           (let ([tmp (unique-name 'func)])
             `(let ([,tmp ,rator])
                (,tmp ,tmp ,rand* ...))))]
      [,x x]))

  (convert pgm))


(define (optimize-known-call pgm)

  (define (extend-env env u* v*)
    (if (null? u*)
        env
        (cons (cons (car u*) (car v*))
              (extend-env env (cdr u*) (cdr v*)))))

  (define (apply-env env u)
    (cond
      [(assq u env) => cdr]
      [else u]))

  (define (optimize x env)
    (match x
      [(letrec ([,label* (lambda (,cp* ,a** ...)
                           (bind-free (,cp* ,free* ...) ,e*))] ...)
         (closures ([,uvar* ,label* ,free* ...] ...) ,e))
       (let ([new-env (extend-env env uvar* label*)])
         `(letrec ([,label* (lambda (,cp* ,a** ...)
                              (bind-free (,cp* ,free* ...)
                                         ,(optimize e* new-env)))] ...)
            (closures ([,uvar* ,label* ,free* ...] ...)
                      ,(optimize e new-env))))]
      [(let ([,uvar* ,[e*]] ...) ,[e])
       `(let ([,uvar* ,e*] ...) ,e)]
      [(,h ,[t*] ...) (guard (uvar? h))
       `(,(apply-env env h) ,t* ...)]
      [(,[h] ,[t*] ...) `(,h ,t* ...)]
      [,x x]))

  (optimize pgm '()))


(define (introduce-procedure-primitives pgm)

  (define (index-of e l)
    (let loop ([l l] [idx 0])
      (cond
        [(null? l) #f]
        [(eq? e (car l)) idx]
        [else (loop (cdr l) (add1 idx))])))

  (define (make-locate bd)
    (lambda (v)
      (cond
        [(index-of v (cdr bd)) =>
         (lambda (i) `(procedure-ref ,(car bd) ',i))]
        [else v])))

  (define (make-procedure-set! uvar free)
    (let loop ([free free] [idx 0])
      (if (null? free)
          '()
          (cons `(procedure-set! ,uvar ',idx ,(car free))
                (loop (cdr free) (add1 idx))))))

  (define (intro x locate)
    (match x
      [(bind-free (,cp ,free ...) ,e)
       (intro e (make-locate `(,cp ,free ...)))]
      [(closures ([,uvar* ,label* ,[free*] ...] ...) ,[e])
       `(let ([,uvar* (make-procedure ,label*
                                      ',(map length free*))]
              ...)
          ,(make-begin
            `(,(map make-procedure-set! uvar* free*) ... ... ,e)))]
      [(letrec ([,label* (lambda (,cp* ,a** ...) ,[e*])] ...) ,[e])
       `(letrec ([,label* (lambda (,cp* ,a** ...) ,e*)] ...) ,e)]
      [(let ([,uvar* ,[e*]] ...) ,[e])
       `(let ([,uvar* ,e*] ...) ,e)]
      [(,h ,[t*] ...) (guard (or (memq h '(if begin quote))
                                 (prim? h)
                                 (label? h)))
       `(,h ,t* ...)]
      [(,cp ,cp ,[rand*] ...)
       (let ([cp^ (intro cp locate)])
         `((procedure-code ,cp^) ,cp^ ,rand* ...))]
      [,uvar (guard (uvar? uvar)) (locate uvar)]
      [,x x]))

  (intro pgm id))


(define (lift-letrec pgm)

  (define dec* '())
  (define (add-dec! label proc)
    (set! dec* (cons (list label proc) dec*)))

  (define (lift x)
    (match x
      [(letrec ([,label* ,[proc*]] ...) ,[body])
       (for-each (lambda (label proc)
                   (add-dec! label proc))
                 label* proc*)
       body]
      [(,[e*] ...) `(,e* ...)]
      [,x x]))

  (let ([body (lift pgm)])
    `(letrec ,(reverse dec*) ,body)))


(define (normalize-context pgm)

  (define (value-prim^? x)
    (or (value-prim? x)
        (memq x '(make-procedure procedure-ref procedure-code))))

  (define (effect-prim^? x)
    (or (effect-prim? x)
        (eq? x 'procedure-set!)))

  (define (value->pred v)
    `(if (eq? ,v '#f) (false) (true)))

  (define (norm ct)
    (lambda (x)
      (match x
        [(letrec ([,label* (lambda (,uvar** ...)
                             ,[(norm 'value) -> body*])] ...)
           ,[body])
         `(letrec ([,label* (lambda (,uvar** ...) ,body*)] ...)
            ,body)]
        [(if ,[(norm 'pred) -> p] ,[x1] ,[x2])
         `(if ,p ,x1 ,x2)]
        [(begin ,[(norm 'effect) -> e*] ... ,[x0])
         (make-nopless-begin `(,e* ... ,x0))]
        [(let ([,uvar* ,[(norm 'value) -> val*]] ...) ,[x0])
         `(let ([,uvar* ,val*] ...) ,x0)]
        [(quote ,imm)
         (case ct
           [value `(quote ,imm)]
           [effect `(nop)]
           [pred (if (eq? imm #f) `(false) `(true))])]
        [(,value-prim ,[(norm 'value) -> rand*] ...)
         (guard (value-prim^? value-prim))
         (let ([value `(,value-prim ,rand* ...)])
           (case ct
             [value value]
             [effect `(nop)]
             [pred (value->pred value)]))]
        [(,pred-prim ,[(norm 'value) -> rand*] ...)
         (guard (pred-prim? pred-prim))
         (let ([prd `(,pred-prim ,rand* ...)])
           (case ct
             [value `(if ,prd '#t '#f)]
             [effect `(nop)]
             [pred prd]))]
        [(,effect-prim ,[(norm 'value) -> rand*] ...)
         (guard (effect-prim^? effect-prim))
         (let ([effect `(,effect-prim ,rand* ...)])
           (case ct
             [value `(begin ,effect (void))]
             [effect effect]
             [pred `(begin ,effect (true))]))]
        [(,[(norm 'value) -> rator] ,[(norm 'value) -> rand*] ...)
         (let ([value `(,rator ,rand* ...)])
           (case ct
             [(value effect) value]
             [pred (value->pred value)]))]
        [,triv
         (guard (or (uvar? triv) (label? triv)))
         (case ct
           [value triv]
           [effect `(nop)]
           [pred (if (uvar? triv) (value->pred triv) `(true))])]
        [,x (error 'normalize-context "invalide expression" x)])))

  ((norm 'value) pgm))


(define (specify-representation pgm)

  (define (trivial? x) (number? x))

  (define (Imm imm)
    (cond
      [(null? imm) $nil]
      [(eq? imm #f) $false]
      [(eq? imm #t) $true]
      [(number? imm) (ash imm shift-fixnum)]
      [else (error "invalid immediate" imm)]))

  (define (trivialize expr tmp-name)
    (if (trivial? expr)
        (values expr '())
        (let ([t (unique-name tmp-name)])
          (values t `((,t ,expr))))))

  (define (make-arith-op op a b)
    (if (and (number? a) (number? b))
        (eval `(,op ,a ,b))
        `(,op ,a ,b)))

  (define (specify x)
    (match x
      [(if ,[p] ,[x1] ,[x2]) `(if ,p ,x1 ,x2)]
      [(begin ,[e*] ... ,[x0]) `(begin ,e* ... ,x0)]
      [(let ([,var* ,[val*]] ...) ,[x0]) `(let ([,var* ,val*] ...) ,x0)]
      [(nop) `(nop)]
      [(false) `(false)]
      [(true) `(true)]
      [(quote ,[Imm -> imm]) imm]
      ; value-prim
      [(car ,[p]) `(mref ,p ,(- disp-car tag-pair))]
      [(cdr ,[p]) `(mref ,p ,(- disp-cdr tag-pair))]
      [(cons ,[a] ,[b])
       (letv*
        ([tmp-pair (unique-name 'tmp-pair)]
         [(a^ ad*) (trivialize a 'tmp-car)]
         [(b^ bd*) (trivialize b 'tmp-cdr)]
         [d* `(,ad* ... ,bd* ...)]
         [body `(let ([,tmp-pair (+ (alloc ,size-pair) ,tag-pair)])
                  (begin
                    (mset! ,tmp-pair ,(- disp-car tag-pair) ,a^)
                    (mset! ,tmp-pair ,(- disp-cdr tag-pair) ,b^)
                    ,tmp-pair))])
        (if (null? d*) body `(let (,d* ...) ,body)))]
      [(vector-length ,[v])
       `(mref ,v ,(- disp-vector-length tag-vector))]
      [(vector-ref ,[v] ,[idx])
       `(mref ,v ,(make-arith-op '+ idx (- disp-vector-data tag-vector)))]
      [(make-vector ,[l])
       (letv*
        ([tmp-vec (unique-name 'tmp-vec)]
         [(l^ ld*) (trivialize l 'tmp-len)]
         [body `(let ([,tmp-vec (+ (alloc ,(make-arith-op '+  disp-vector-data l^))
                                   ,tag-vector)])
                  (begin
                    (mset! ,tmp-vec ,(- disp-vector-length tag-vector) ,l^)
                    ,tmp-vec))])
        (if (null? ld*) body `(let (,ld* ...) ,body)))]
      [(procedure-code ,[cp])
       `(mref ,cp ,(- disp-procedure-code tag-procedure))]
      [(procedure-ref ,[cp] ,[idx])
       `(mref ,cp ,(make-arith-op '+ idx (- disp-procedure-data tag-procedure)))]
      [(make-procedure ,[label] ,[n])
       (let ([tmp (unique-name 'tmp-proc)])
         `(let ([,tmp (+ (alloc ,(make-arith-op '+ disp-procedure-data n))
                         ,tag-procedure)])
            (begin
              (mset! ,tmp ,(- disp-procedure-code tag-procedure) ,label)
              ,tmp)))]
      [(void) $void]
      [(* ,[a] ,[b])
       (cond
         [(and (trivial? a) (trivial? b)) (* (sra a shift-fixnum) b)]
         [(trivial? a) `(* ,(sra a shift-fixnum) ,b)]
         [(trivial? b) `(* ,a ,(sra b shift-fixnum))]
         [else `(* (sra ,a ,shift-fixnum) ,b)])]
      [(,binop ,[a] ,[b]) (guard (binop? binop)) `(,binop ,a ,b)]
      ; pred-prim
      [(boolean? ,[v]) `(= (logand ,v ,mask-boolean) ,tag-boolean)]
      [(fixnum? ,[v]) `(= (logand ,v ,mask-fixnum) ,tag-fixnum)]
      [(pair? ,[v]) `(= (logand ,v ,mask-pair) ,tag-pair)]
      [(vector? ,[v]) `(= (logand ,v ,mask-vector) ,tag-vector)]
      [(procedure? ,[v]) `(= (logand ,v ,mask-procedure) ,tag-procedure)]
      [(null? ,[v]) `(= ,v ,$nil)]
      [(eq? ,[a] ,[b]) `(= ,a ,b)]
      [(,relop ,[a] ,[b]) (guard (relop? relop)) `(,relop ,a ,b)]
      ; effect-prim
      [(set-car! ,[p] ,[v]) `(mset! ,p ,(- disp-car tag-pair) ,v)]
      [(set-cdr! ,[p] ,[v]) `(mset! ,p ,(- disp-cdr tag-pair) ,v)]
      [(vector-set! ,[vec] ,[idx] ,[v])
       `(mset! ,vec ,(make-arith-op '+ idx (- disp-vector-data tag-vector)) ,v)]
      [(procedure-set! ,[cp] ,[idx] ,[v])
       `(mset! ,cp
               ,(make-arith-op '+ idx (- disp-procedure-data tag-procedure))
               ,v)]
      ; app
      [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
      [,triv (guard (or (uvar? triv) (label? triv))) triv]
      [,x (error 'specify-representation "invalid expression" x)]))

  (match pgm
    [(letrec ([,label* (lambda ,uvar** ,[specify -> body*])] ...)
       ,[specify -> body])
     `(letrec ([,label* (lambda ,uvar** ,body*)] ...) ,body)]))


(define (remove-let pgm)

  (define (Body body)

    (define local* '())

    (define (add-local uvar)
      (when (memq uvar local*)
        (error 'remove-let "multiple definitions" uvar))
      (set! local* (cons uvar local*)))

    (define (rm x)
      (match x
        [(begin ,[e*] ...) (make-begin e*)]
        [(let ([,uvar* ,[val*]] ...) ,[x0])
         (for-each (lambda (uvar) (add-local uvar)) uvar*)
         (make-begin `((set! ,uvar* ,val*) ... ,x0))]
        [(,[e*] ...) `(,e* ...)]
        [,x x]))

    (let ([new-body (rm body)])
      `(locals ,local* ,new-body)))

  (match pgm
    [(letrec ([,label* (lambda ,uvar** ,[Body -> body*])] ...)
       ,[Body -> body])
     `(letrec ([,label* (lambda ,uvar** ,body*)] ...)
        ,body)]))


;;;


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
    (filter (lambda (v) (or (uvar? v) (target? v)))
            (match triv
              [(mref ,base ,offset) (list base offset)]
              [,triv (list triv)])))

  (define (uncover-set var live* triv-live*)
    (let ([new-live* (difference live* `(,var))])
      (when (uvar? var)
        (add-conflict var new-live*))
      (when (or (uvar? var) (target? var))
        (for-each (lambda (live)
                    (when (uvar? live)
                      (add-conflict live `(,var))))
                  new-live*))
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
      [(set! (mref ,[Triv -> base-live*] ,[Triv -> offset-live*])
             (,binop ,[Triv -> triv*-live*] ...))
       (apply union live* base-live* offset-live* triv*-live*)]
      [(set! (mref ,[Triv -> base-live*] ,[Triv -> offset-live*])
             ,[Triv -> triv-live*])
       (union live* base-live* offset-live* triv-live*)]
      [(set! ,var (,binop ,[Triv -> triv*-live*] ...))
       (uncover-set var live* (apply union triv*-live*))]
      [(set! ,var ,[Triv -> triv-live*])
       (uncover-set var live* triv-live*)]
      [(return-point ,rp-label ,[lv*]) (add-call-live* live*) lv*]
      [(true) true-live*]
      [(false) false-live*]
      [(,relop ,[Triv -> triv*-live*] ...)
       (guard (relop? relop))
       (apply union true-live* false-live* triv*-live*)]
      [(,[Triv -> triv-live*] ,loc* ...)
       (uncover-set return-value-register
                    live*
                    (union (filter target? loc*) triv-live*))]))

  (define (check-live-set live*)
    (let ([uvar* (filter uvar? live*)])
      (unless (null? uvar*)
        (error 'check-live-set "uninitialized var" uvar*))))

  (begin
    (check-live-set (live-analysis tail '() #f))
    (values conflicts call-live*)))


(define (map-uil-body func pgm)
  (match pgm
    [(letrec ([,label* (lambda ,arg** ,[func -> body*])] ...)
       ,[func -> body])
     `(letrec ([,label* (lambda ,arg** ,body*)] ...) ,body)]))


(define (verify-uil pgm)
  (define who 'verify-uil)

  (define (verify-x-list x* x? name)
    (unless (list? x*)
      (error who (format  "expect a list of ~a but get ~a" name x*)))
    (let loop ([x* x*])
      (unless (null? x*)
        (if (x? (car x*))
            (loop (cdr x*))
            (error who (format "invalid ~a" name) (car x*))))))

  (define (verify-tail tail label* uvar*)

    (define (Tail tail)
      (match tail
        [(if ,[Pred -> p] ,[t1] ,[t2])
         (void)]
        [(begin ,[Effect -> e*] ... ,[t])
         (void)]
        [(alloc ,[Value -> v])
         (void)]
        [(mref ,[Value -> base] ,[Value -> offset])
         (void)]
        [(,binop ,[Value -> v1] ,[Value -> v2]) (guard (binop? binop))
         (void)]
        [(,[Value -> rator] ,[Value -> rand*] ...)
         (void)]
        [,triv (guard (not (pair? triv)))
         (Triv triv)]
        [,x (error who "invalid Tail" x)]))

    (define (Pred prd)
      (match prd
        [(true) (void)]
        [(false) (void)]
        [(if ,[p] ,[p1] ,[p2])
         (void)]
        [(begin ,[Effect -> e*] ... ,[p])
         (void)]
        [(,relop ,[Value -> v1] ,[Value -> v2]) (guard (relop? relop))
         (void)]
        [,x (error who "invalid Pred" x)]))

    (define (Effect effect)
      (match effect
        [(nop) (void)]
        [(set! ,[Var -> uvar] ,[Value -> v])
         (void)]
        [(mset! ,[Value -> base] ,[Value -> offect] ,[Value -> v])
         (void)]
        [(if ,[Pred -> p] ,[e1] ,[e2])
         (void)]
        [(begin ,[e*] ... ,[e])
         (void)]
        [(,[Value -> rator] ,[Value -> rand*] ...)
         (void)]
        [,x (error who "invalid Effect" x)]))

    (define (Value value)
      (match value
        [(alloc ,[v]) (void)]
        [(mref ,[base] ,[offect]) (void)]
        [(if ,[Pred -> p] ,[v1] ,[v2])
         (void)]
        [(begin ,[Effect -> e*] ... ,[v])
         (void)]
        [(,binop ,[v1] ,[v2]) (guard (binop? binop))
         (void)]
        [(,[Value -> rator] ,[Value -> rand*] ...)
         (void)]
        [,triv (guard (not (pair? triv)))
         (Triv triv)]
        [,x (error who "invalid Value" x)]))

    (define (Triv triv)
      (cond
        [(uvar? triv)
         (unless (memq triv uvar*)
           (error who "reference to unbound uvar" triv))]
        [(label? triv)
         (unless (memq triv label*)
           (error who "unbound label" triv))]
        [(number? triv)
         (unless (and (integer? triv) (exact? triv))
           (error who "not a integer" triv))
         (unless (int64? triv)
           (error who "integer out of 64-bit range" triv))]
        [else (error who "invalid Triv" triv)]))

    (define (Var uvar)
      (unless (uvar? uvar)
        (error who "not a uvar" uvar)))

    (Tail tail))

  (define (Body body label* arg*)
    (match body
      [(locals ,local* ,tail)
       (verify-x-list local* uvar? 'uvar)
       (verify-tail tail label* (append arg* local*))]
      [,x (error who "invalid Body" x)]))

  (match pgm
    [(letrec ([,label* (lambda (,arg** ...) ,body*)] ...) ,body)
     (verify-x-list label* label? 'label)
     (for-each
      (lambda (label arg* body)
        (verify-x-list arg* uvar? (format "formal in ~a" label))
        (Body body label* arg*))
      label* arg** body*)
     (Body body label* '())]
    [,x (error who "invalid Program" x)])

  pgm)


; Simple View:
; E -> Triv
;    | (E E*)
;    | (prim E*)
;    | (if E E E)
;    | (begin E* E)
;    | (set! uvar E)
;    | (mset! E E E)
;    | (nop)
;    | (true)
;    | (false)
; prim -> binop | relop | alloc | mref | mset!
;
; Flatten set!:
; F[X] -> (if P X X)
;       | (begin E* X)
; T -> Triv
;    | (binop Triv Triv)
;    | (Triv Triv*)
;    | F[T]
; E -> (nop)
;    | (set! lhs Triv)
;    | (set! lhs (value-prim Triv Triv))
;    | (return-point label (Triv Triv*))
;    | (set! lhs (return-point label (Triv Triv*)))
;    | F[E]
; P -> (true)
;    | (false)
;    | (relop Triv Triv)
;    | F[P]
; Triv -> TT | MRef
; MRef -> (mref TT TT)
; TT -> uvar | int | label
; lhs -> uvar | MRef

(define (remove-complex-opera* pgm)

  (define (Tail tail)
    (define new-uvar* '())
    (define (new-temp-var)
      (let ([u (unique-name 't)])
        (set! new-uvar* (cons u new-uvar*))
        u))
    (define (return x) (lambda (ctx) (ctx x)))
    (define (set!-ctx uvar) (lambda (s) `(set! ,uvar ,s)))
    (define (with-u make-effect ctx)
      (let ([u (new-temp-var)])
        (make-begin (list (make-effect u) (ctx u)))))

    ; ct: tail rhs seq test alloc arg
    (define (rm ct)
      (lambda (x)
        (match x
          [(nop) (return `(nop))]
          [(true) (return `(true))]
          [(false) (return `(false))]
          [(set! ,lhs ,[(rm 'rhs) -> v])
           (return (v (set!-ctx lhs)))]
          [(mset! ,[(rm 'arg) -> base] ,[(rm 'arg) -> offset]
                  ,[(rm 'rhs) -> v])
           (return
            (do/k
             (base <- (base))
             (offset <- (offset))
             (s <- (v))
             `(set! (mref ,base ,offset) ,s)))]
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
          [(alloc ,[(rm 'alloc) -> v])
           (lambda (ctx)
             (do/k
              (s <- (v))
              (with-u (lambda (u)
                        (make-begin
                         `((set! ,u ,allocation-pointer-register)
                           (set! ,allocation-pointer-register
                                 (+ ,allocation-pointer-register ,s)))))
                ctx)))]
          [(,op ,[(rm 'arg) -> v1] ,[(rm 'arg) -> v2])
           (guard (or (binop? op) (relop? op) (eq? op 'mref)))
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
          [(,[(rm 'arg) -> rator] ,[(rm 'arg) -> rand*] ...)
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
          [,uvar
           (guard (and (uvar? uvar) (eq? ct 'arg)))
           (lambda (ctx)
             (with-u (lambda (u) `(set! ,u ,uvar)) ctx))]
          [,triv
           (guard (or (uvar? triv) (number? triv) (label? triv)))
           (return triv)]
          [,x (error 'remove-complex-opera* "invalid expression" x)])))
    (values (reverse new-uvar*) (((rm 'tail) tail) id)))

  (define (Body body)
    (match body
      [(locals ,uvar* ,[Tail -> u* tail])
       `(locals (,uvar* ... ,u* ...) ,tail)]))

  (map-uil-body Body pgm))


(define (impose-calling-conventions pgm)

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

  (define rp (unique-name 'rp))

  (define (make-app rator rand*)
    `(,rator ,frame-pointer-register
             ,allocation-pointer-register
             ,return-address-register
             ,@rand*))

  (define (make-ret v)
    `(begin (set! ,return-value-register ,v)
            (,rp ,frame-pointer-register
                 ,allocation-pointer-register
                 ,return-value-register)))

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
                            ,(make-app rator param*))))]
        [(set! ,lhs (return-point ,rp-label ,app))
         `(begin ,(impose `(return-point ,rp-label ,app))
                 (set! ,lhs ,return-value-register))]
        [(set! ,lhs ,value) `(set! ,lhs ,value)]
        [(nop) '(nop)]
        [(true) '(true)]
        [(false) '(false)]
        [(,relop ,triv1 ,triv2) (guard (relop? relop))
         `(,relop ,triv1 ,triv2)]
        [(,op ,triv* ...) (guard (or (eq? op 'mref) (binop? op)))
         (make-ret `(,op ,triv* ...))]
        [(,rator ,rand* ...)
         (let ([loc* (alloc-loc* (length rand*))])
           `(begin
              (set! ,return-address-register ,rp)
              (set! ,loc* ,rand*) ...
              ,(make-app rator loc*)))]
        [,triv (make-ret triv)]))
    (let ([tail (impose tail)])
      (values new-frames tail)))

  (match pgm
    [(letrec ([,label* (lambda ,args*
                         (locals ,uvar**
                                 ,[Tail -> new-frames* tail*]))] ...)
       (locals ,u* ,[Tail -> new-frames tail]))
     `(letrec ([,label* (lambda ()
                          ,(map (lambda (uvar* args new-frames tail)
                                  `(locals
                                    (,uvar* ... ,rp ,args ... ,new-frames ... ...)
                                    (new-frames
                                     ,new-frames
                                     ,(let ([loc* (alloc-loc* (length args))])
                                        (make-begin
                                         `((set! ,rp ,return-address-register)
                                           (set! ,args ,loc*) ...
                                           ,tail))))))
                                uvar**
                                args*
                                new-frames*
                                tail*))] ...)
        (locals (,u* ... ,rp ,new-frames ... ...)
                (new-frames ,new-frames
                            ,(make-begin
                              `((set! ,rp ,return-address-register)
                                ,tail)))))]))


(define (forward-propagate x target?)

  (define (appenv env a)
    (cond
      [(assq a env) => cdr]
      [else a]))

  (define (extenv a b env)
    (cons (cons a b) env))

  (define (eliminate a env)
    (filter (lambda (p) (not (or (eq? (car p) a) (eq? (cdr p) a))))
            env))

  (define (remove-loc* env)
    (define (loc? x) (or (register? x) (frame-var? x)))
    (filter (lambda (p) (not (or (loc? (car p)) (loc? (cdr p)))))
            env))

  (define (intersect e1 e2)
    (cond
      [(null? e1) '()]
      [(null? e2) '()]
      [(member (car e1) e2)
       (cons (car e1) (intersect (cdr e1) e2))]
      [else (intersect (cdr e1) e2)]))

  (define (forward x env)
    (match x
      [(set! ,a ,b)
       (letv*
        ([(b^ env1) (forward b env)])
        (values `(set! ,a ,b^)
                (let ([ee (eliminate a env1)])
                  (if (and (target? a) (not (pair? b^)))
                      (extenv a b^ ee)
                      ee))))]
      [(begin ,x0) (forward x0 env)]
      [(begin ,e ,e* ... ,x0)
       (letv*
        ([(e^ env1) (forward e env)]
         [(e*^ env2) (forward `(begin ,e* ... ,x0) env1)])
        (values (make-begin `(,e^ ,e*^)) env2))]
      [(if ,p ,x1 ,x2)
       (letv*
        ([(p^ env0) (forward p env)]
         [(x1^ env1) (forward x1 env0)]
         [(x2^ env2) (forward x2 env0)])
        (values `(if ,p^ ,x1^ ,x2^) (intersect env1 env2)))]
      [(,h ,t* ...)
       (guard (or (memq h '(return-point nop false true mref))
                  (relop? h)
                  (binop? h)))
       (let loop ([t* t*] [env env] [acc '()])
         (if (null? t*)
             (values (cons h (reverse acc)) env)
             (letv*
              ([(t^ envt) (forward (car t*) env)])
              (loop (cdr t*) envt (cons t^ acc)))))
       ]
      [(,h ,t* ...)
       (letv*
        ([(h^ envh) (forward h env)])
        (let loop ([t* t*] [env envh] [acc '()])
          (if (null? t*)
              (values (cons h^ (reverse acc)) (remove-loc* env))
              (letv*
               ([(t^ envt) (forward (car t*) env)])
               (loop (cdr t*) envt (cons t^ acc))))))]
      [,x (values (appenv env x) env)]))

  (letv* ([(x^ env) (forward x '())]) x^))


(define (backward-delete x)

  (define (Triv triv)
    (values
     triv
     (filter (lambda (v) (or (uvar? v) (register? v) (frame-var? v)))
             (match triv
               [(mref ,base ,offset) (list base offset)]
               [,triv (list triv)]))))

  (define (set-live* var live* triv-live*)
    (let ([new-live* (difference live* `(,var))])
      (union new-live* triv-live*)))

  (define (backward x live* false-live*)
    (define true-live* live*)
    (match x
      [(if ,prd ,[x1 true-live*] ,[x2 false-live*])
       (letv*
        ([(prd live*) (backward prd true-live* false-live*)])
        (values `(if ,prd ,x1 ,x2) live*))]
      [(begin ,[x0 lv*]) (values x0 lv*)]
      [(begin ,effect* ... ,[x0 lv*])
       (letv*
        ([(ef* live*) (backward `(begin ,effect* ...) lv* #f)])
        (values (make-nopless-begin `(,ef* ,x0)) live*))]
      [(nop) (values `(nop) live*)]
      [(set! (mref ,[Triv -> base base-live*]
                   ,[Triv -> offset offset-live*])
             (,binop ,[Triv -> triv* triv*-live*] ...))
       (values
        `(set! (mref ,base ,offset) (,binop ,triv* ...))
        (apply union live* base-live* offset-live* triv*-live*))]
      [(set! (mref ,[Triv -> base base-live*]
                   ,[Triv -> offset offset-live*])
             ,[Triv -> triv triv-live*])
       (values
        `(set! (mref ,base ,offset) ,triv)
        (union live* base-live* offset-live* triv-live*))]
      [(set! ,var (,binop ,[Triv -> triv* triv*-live*] ...))
       (if (memq var live*)
           (values
            `(set! ,var (,binop ,triv* ...))
            (set-live* var live* (apply union triv*-live*)))
           (values `(nop) live*))]
      [(set! ,var ,[Triv -> triv triv-live*])
       (if (memq var live*)
           (values
            `(set! ,var ,triv)
            (set-live* var live* triv-live*))
           (values `(nop) live*))]
      [(return-point ,rp-label ,[tail lv*])
       (values `(return-point ,rp-label ,tail) lv*)]
      [(true) (values `(true) true-live*)]
      [(false) (values `(false) false-live*)]
      [(,relop ,[Triv -> triv* triv*-live*] ...)
       (guard (relop? relop))
       (values
        `(,relop ,triv* ...)
        (apply union true-live* false-live* triv*-live*))]
      [(,[Triv -> triv triv-live*] ,loc* ...)
       (values
        `(,triv ,loc* ...)
        (set-live* return-value-register
                     live*
                     (union loc* triv-live*)))]))

  (letv* ([(x^ live*) (backward x '() #f)]) x^))


(define (forward-locations pgm)
  (map-uil-body
   (lambda (body)
     (match body
       [(locals
         ,uvar*
         (new-frames
          ,new-frame* ,tail))
        (let ([target?
               (let ([nfv* `(,new-frame* ... ...)])
                 (lambda (x)
                   (not (or (register? x)
                            (frame-var? x)
                            (memq x nfv*)))))])
          `(locals
            ,uvar*
            (new-frames
             ,new-frame*
             ,(backward-delete
               (forward-propagate tail target?)))))]))
   pgm))


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
         (if (= nb 0)
             `(return-point ,rp-label (begin ,e* ... ,app))
             `(begin (return-point ,rp-label
                                   (begin
                                     ,e* ...
                                     (set! ,fp (+ ,fp ,nb))
                                     ,app))
                     (set! ,fp (- ,fp ,nb)))))]
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


(define (select-instructions pgm)

  (define (mem-or-label? v) (or (mem? v) (label? v)))

  (define (Tail tail)

    (define unspill* '())

    (define (temp-var)
      (let ([u (unique-name 'u)])
        (set! unspill* (cons u unspill*))
        u))

    (define (make-relop relop a b)
      (if (and (number? b) (not (int32? b)))
          (let ([u (temp-var)])
            `(begin (set! ,u ,b)
                    (,relop ,a ,u)))
          `(,relop ,a ,b)))

    (define (Triv triv ct ctx)
      (define (ctx/u ef* v)
        (let ([u (temp-var)])
          (ctx `(,@ef* (set! ,u ,v)) u)))
      (match triv
        [(mref ,base ,offset)
         (do/k
          (base-ef* base <- (Triv base 'mref))
          (offset-ef* offset <- (Triv offset 'mref))
          ((if ct ctx/u ctx)
           `(,base-ef* ... ,offset-ef* ...)
           `(mref ,base ,offset)))]
        [,triv
         ((if (and ct (mem-or-label? triv))
              ctx/u
              ctx)
          '()
          triv)]))

    (define (select/k x k)
      (define (>>) (k x))
      (match x
        [(if ,p ,x1 ,x2)
         (do/k
          (p <- (select/k p))
          (x1 <- (select/k x1))
          (x2 <- (select/k x2))
          (k `(if ,p ,x1 ,x2)))]
        [(begin ,x0) (select/k x0 k)]
        [(begin ,ef ,ef* ... ,x0)
         (do/k
          (ef <- (select/k ef))
          (ef* <- (select/k `(begin ,ef* ... ,x0)))
          (k (make-begin (list ef ef*))))]
        [(nop) (>>)]
        [(set! ,lhs (,binop ,triv1 ,triv2)) (guard (binop? binop))
         (k (if (equal? lhs triv1)
                (do/k
                 (lhs-ef* lhs <- (Triv lhs #f))
                 (triv2-ef* triv2 <- (Triv triv2 (mem? lhs)))
                 (make-begin
                  `(,triv2-ef*
                    ...
                    ,lhs-ef*
                    ...
                    (set! ,lhs (,binop ,lhs ,triv2)))))
                (do/k
                 (lhs-ef* lhs <- (Triv lhs #f))
                 (triv1-ef* triv1 <- (Triv triv1 #f))
                 (triv2-ef* triv2 <- (Triv triv2 #f))
                 (let ([u (temp-var)])
                   (make-begin
                    `(,triv1-ef*
                      ...
                      (set! ,u ,triv1)
                      ,triv2-ef*
                      ...
                      (set! ,u (,binop ,u ,triv2))
                      ,lhs-ef*
                      ...
                      (set! ,lhs ,u)))))))]
        [(set! ,lhs ,triv)
         (k (do/k (lhs-ef* lhs <- (Triv lhs #f))
                  (triv-ef* triv <- (Triv triv (mem? lhs)))
                  (make-begin `(,triv-ef* ... ,lhs-ef* ...
                                          (set! ,lhs ,triv)))))]
        [(return-point ,rp-label ,tail)
         (do/k (tail <- (select/k tail))
               (k `(return-point ,rp-label ,tail)))]
        [(true) (>>)]
        [(false) (>>)]
        [(,relop ,triv1 ,triv2) (guard (relop? relop))
         (k (do/k (triv1-ef* triv1 <- (Triv triv1 #f))
                  (triv2-ef* triv2 <- (Triv triv2 #f))
                  (if (or (number? triv1)
                          (and (mem? triv1) (mem? triv2)))
                      (let ([u (temp-var)])
                        (make-begin
                         `(,triv1-ef*
                           ...
                           (set! ,u ,triv1)
                           ,triv2-ef*
                           ...
                           ,(make-relop relop u triv2))))
                      (make-begin
                       `(,triv1-ef* ... ,triv2-ef* ...
                                    ,(make-relop relop triv1 triv2))))))]
        [(,triv ,loc* ...)
         (do/k (ef* triv <- (Triv triv #f))
               (k (make-begin `(,ef* ... (,triv ,loc* ...)))))]))

    (let ([tail (select/k tail id)])
      (values (reverse unspill*) tail)))

  (define (Body body)
    (match body
      [(locals
        ,uvar*
        (ulocals
         ,unspill*
          (locate
           ,location*
           (frame-conflict ,fc-graph ,[Tail -> u* tail]))))
       `(locals
         ,uvar*
         (ulocals
          (,unspill* ... ,u* ...)
          (locate
           ,location*
           (frame-conflict ,fc-graph ,tail))))]
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

  ; very simple assign (linear assign)
  (define (assign  unspill* rc-graph)
    (define (unspill? uvar) (memq uvar unspill*))
    (define (select-register regs conflicts)
      (if (null? regs)
          #f
          (if (memq (car regs) conflicts)
              (select-register (cdr regs) conflicts)
              (car regs))))
    (define (reorder graph)
      ; make unspill* before the others
      (let-values ([(u* s*) (partition (lambda (r) (unspill? (car r)))
                                       graph)])
        (append u* s*)))
    (define (expose-conflicts conflicts mapping)
      (let loop ([c* conflicts])
        (if (null? c*)
            '()
            (let ([v (car c*)])
              (if (uvar? v)
                  (let ([a (assq v mapping)])
                    (if a
                        (cons (cadr a) (loop (cdr c*)))
                        (loop (cdr c*))))
                  (cons v (loop (cdr c*))))))))
    (let loop ([g (reorder rc-graph)] [res '()])
      (if (null? g)
          (values (reverse res) '())
          (let ([uvar (caar g)]
                [reg (select-register registers
                                      (expose-conflicts (cdar g) res))])
            (if reg
                (loop (cdr g) (cons (list uvar reg) res))
                (if (unspill? uvar)
                    (error 'assign-registers "not enough registers" uvar)
                    (values #f (map car g))))))))

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
    [(set! ,[lhs] (,binop ,[triv*] ...))
     `(set! ,lhs (,binop ,triv* ...))]
    [(set! ,[lhs] ,[triv])
     (if (equal? lhs triv)
         `(nop)
         `(set! ,lhs ,triv))]
    [(return-point ,rp-label ,[tail]) `(return-point ,rp-label ,tail)]
    [(nop) `(nop)]
    [(false) `(false)]
    [(true) `(true)]
    [(mref ,[base] ,[offset]) `(mref ,base ,offset)]
    [(,relop ,[triv*] ...) (guard (relop? relop))
     `(,relop ,triv* ...)]
    [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
    [,v (guard (uvar? v)) (lookup v env)]
    [,x (guard (or (register? x) (frame-var? x) (label? x) (number? x))) x]
    [,x (error 'finalize "invalid exp" x)]))


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
      [(set! ,lhs ,rhs) `(set! ,lhs ,rhs)]
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


(define (expose-mem-var pgm)

  (define fp-offset 0)

  (define (expose x)
    (match x
      [(if ,[p] ,[x1] ,[x2]) `(if ,p ,x1 ,x2)]
      [(begin ,[x0]) `(begin ,x0)]
      [(begin ,[ef] ,ef* ... ,x0)
       (make-begin `(,ef ,(expose `(begin ,ef* ... ,x0))))]
      [(set! ,fp (,binop ,a ,b)) (guard (eq? fp frame-pointer-register))
       (let ([ef `(set! ,fp (,binop ,a ,b))])
         (unless (and (eq? fp a) (number? b) (binop? binop))
           (error 'expose-frame-var "invalid fp setting" ef))
         (set! fp-offset ((eval binop) fp-offset b))
         ef)
       ]
      [(set! ,[lhs] (,binop ,[a] ,[b])) (guard (binop? binop))
       `(set! ,lhs (,binop ,a ,b))]
      [(set! ,[lhs] ,[rhs]) `(set! ,lhs ,rhs)]
      [(return-point ,rp-label ,[tail])
       `(return-point ,rp-label ,tail)]
      [(nop) `(nop)]
      [(false) `(false)]
      [(true) `(true)]
      [(,relop ,[a] ,[b]) (guard (relop? relop))
       `(,relop ,a ,b)]
      [(,[triv]) `(,triv)]
      [(mref ,base ,offset)
       (cond
         [(and (register? base) (register? offset))
          (make-index-opnd base offset)]
         [(and (register? base) (number? offset))
          (make-disp-opnd base offset)]
         [(and (number? base) (register? offset))
          (make-disp-opnd offset base)]
         [else
          (error 'expose-heap-var "invalid mref" base offset)])]
      [,triv
       (guard (frame-var? triv))
       (make-disp-opnd frame-pointer-register
                       (- (fxsll (frame-var->index triv) align-shift)
                          fp-offset))]
      [,triv triv]))

  (define (Body body)
    (set! fp-offset 0)
    (expose body))

  (define (Dec dec)
    (match dec
      [(,label (lambda () ,[Body -> tail]))
       `(,label (lambda () ,tail))]))

  (match pgm
    [(letrec (,[Dec -> dec*] ...) ,[Body -> tail])
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


(define (optimize-jumps pgm)

  (define (walk l s)
    (cond
      [(assq l s) => (lambda (p) (walk (cdr p) s))]
      [else l]))

  (define (optimize x s)
    (match x
      [(letrec ([,label* (lambda () ,[tail*])] ...) ,[tail])
       `(letrec ([,label* (lambda () ,tail*)] ...) ,tail)]
      [(,[e*] ...) `(,e* ...)]
      [,l (guard (label? l)) (walk l s)]
      [,triv triv]))

  (match pgm
    [(letrec ([,label* ,body*] ...) ,tail)
     (let loop ([label* label*] [body* body*] [s '()] [dec* '()])
       (if (null? label*)
           (optimize `(letrec ,(reverse dec*) ,tail) s)
           (match (car body*)
             [(lambda () (,to)) (guard (label? to))
              (loop (cdr label*)
                    (cdr body*)
                    (cons (cons (car label*) to) s)
                    dec*)]
             [,body
              (loop (cdr label*)
                    (cdr body*)
                    s
                    (cons `(,(car label*) ,body) dec*))])))]))


(define (flatten-program pgm)

  (define (Tail tail next-label)
    (match tail
      [(begin ,effect* ... ,[tl])
       `(,effect* ... ,tl ...)]
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


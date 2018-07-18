; Program -> (letrec ([label (lambda () Body)]*) Body)
; Body -> (locals (uvar*) Tail)
; Tail -> (Triv Loc*)
;       | (if Pred Tail Tail)
;       | (begin Effect* Tail)
; Pred -> (true)
;       | (false)
;       | (relop Triv Triv)
;       | (if Pred Pred Pred)
;       | (begin Effect* Pred)
; Effect -> (nop)
;         | (set! Var Triv)
;         | (set! Var (binop Triv Triv))
;         | (if Pred Effect Effect)
;         | (begin Effect* Effect)
; Loc -> reg | fvar
; Var -> uvar | Loc
; Triv -> Var | int | label
; reg -> rax | rcx | rdx | rbx |rbp | rsi | rdi
;      | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
; binop -> + | - | * | sra | logand | logor
; relop -> = | > | < | >= | <=
;

(case-sensitive #t)

(load "../lib/match.ss")
(load "../lib/helpers.ss")

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

(define (skip-used-name name*)
  (unless (null? name*)
    (let ([max-count (apply max (map (lambda (l)
                                       (string->number (extract-suffix l)))
                                     name*))])
      (let loop ()
        (unless (> (unique-name-count) max-count)
          (begin
            (unique-name 'skip)
            (loop)))))))

(define (binop? x)
  (memq x '(+ - * sra logand logor)))

(define (relop? x)
  (memq x '(= > < >= <=)))

;;;

(define (verify-scheme pgm) pgm)


(define (uncover-target-conflict tail target?)
  (define conflicts '())
  (define (add-conflict var live*)
    (let ([ass (assq var conflicts)])
      (if ass
          (set-cdr! ass (union live* (cdr ass)))
          (set! conflicts (cons (cons var live*) conflicts)))))

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
      [(true) true-live*]
      [(false) false-live*]
      [(,relop ,[Triv -> triv1-live*] ,[Triv -> triv2-live*])
       (guard (relop? relop))
       (union true-live* false-live* triv1-live* triv2-live*)]
      [(,[Triv -> triv-live*] ,loc* ...)
       (union live* (filter target? loc*) triv-live*)]))

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
    (normalize conflicts)))


(define (map-body func pgm)
  (match pgm
    [(letrec ([,label* (lambda () ,[func -> body*])] ...)
       ,[func -> body])
     `(letrec ([,label* (lambda () ,body*)] ...) ,body)]))


(define (uncover-frame-conflict pgm)
  (map-body (lambda (body)
              (match body
                [(locals ,uvar* ,tail)
                 `(locals ,uvar*
                          (frame-conflict
                           ,(uncover-target-conflict tail frame-var?)
                           ,tail))]))
            pgm))


(define (introduce-allocation-forms pgm)
  (map-body (lambda (body)
              (match body
                [(locals ,uvar* ,e)
                 `(locals ,uvar*
                          (ulocals ()
                                   (locate () ,e)))]))
            pgm))


(define (select-instructions pgm)

  (define (select/k x unspill* cont)
    ; TODO: switch opands
    (define (cont-u trunk)
      (let ([u (unique-name 't)])
        (cont (trunk u) (cons u unspill*))))
    (define (>>) (cont x unspill*))
    (match x
      [(if ,prd ,x1 ,x2)
       (select/k prd unspill*
                 (lambda (prd unspill*)
                   (select/k x1 unspill*
                             (lambda (x1 unspill*)
                               (select/k x2 unspill*
                                         (lambda (x2 unspill*)
                                           (cont `(if ,prd ,x1 ,x2)
                                                 unspill*)))))))]
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
               (if (and (not (frame-var? triv1)) (not (frame-var? triv2)))
                   (cont `(begin (set! ,var ,triv1)
                                 (set! ,var (,binop ,var ,triv2)))
                         unspill*)
                   (cont-u (lambda (u)
                             `(begin (set! ,u ,triv1)
                                     (set! ,u (,binop ,u ,triv2))
                                     (set! ,var ,u))))))
           (if (eq? var triv1)
               (>>)
               (cont `(begin (set! ,var ,triv1)
                             (set! ,var (,binop ,var ,triv2)))
                     unspill*)))]
      [(set! ,var ,triv)
       (if (and (frame-var? var)
                (not (number? triv))
                (not (register? triv))
                (not (uvar? triv)))
           (cont-u (lambda (u)
                     `(begin (set! ,u ,triv)
                             (set! ,var ,u))))
           (>>))]
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
       (skip-used-name uvar*)
       (select/k tail unspill* (lambda (tail unspill*)
                                 `(locals
                                   ,uvar*
                                   (ulocals
                                    ,unspill*
                                    (locate
                                     ,location*
                                     (frame-conflict ,fc-graph ,tail))))))]
      [,body body]))

  (map-body Body pgm))


(define (uncover-register-conflict pgm)
  (map-body (lambda (body)
              (match body
                [(locals ,uvar*
                  (ulocals ,unspill*
                   (locate ,location*
                    (frame-conflict ,fc-graph ,tail))))
                 `(locals ,uvar*
                   (ulocals ,unspill*
                    (locate ,location*
                     (frame-conflict ,fc-graph
                      (register-conflict
                       ,(uncover-target-conflict tail register?)
                       ,tail)))))]
                [,body body]))
            pgm))


(define (assign-registers pgm)

  ; very simple assign (linear assign)
  (define (assign  unspill* rc-graph)
    (define (select-register regs conflicts)
      (if (null? regs)
          #f
          (if (memq (car regs) conflicts)
              (select-register (cdr regs) conflicts)
              (car regs))))
    (define (select-spills uvar* unspill* n)
      (let ([spillable* (filter (lambda (v)
                                  (not (memq v unspill*)))
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

  (map-body Body pgm))


(define (everybody-home? pgm)
  ; todo: more beautiful implementation
  (define ret #t)
  (define (Body body)
    (set! ret (and ret (eq? (car body) 'locate)))
    body)
  (map-body Body pgm)
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

  (map-body Body pgm))


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
    [(,relop ,[triv1] ,[triv2]) `(,relop ,triv1 ,triv2)]
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
  (map-body Body pgm))


(define (discard-call-live pgm)

  (define (Tail tail)
    (match tail
      [(if ,prd ,[tail1] ,[tail2])
       `(if ,prd ,tail1 ,tail2)]
      [(begin ,effect* ... ,[tl])
       `(begin ,effect* ... ,tl)]
      [(,triv ,loc* ...) `(,triv)]))

  (define (Body body)
    (match body
      [(locate ,locates ,[Tail -> tail])
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
      [(begin ,[Effect -> effect*] ... ,[tl])
       `(begin ,effect* ... ,tl)]
      [(if ,[Pred -> prd] ,[tail1] ,[tail2])
       `(if ,prd ,tail1 ,tail2)]
      [(,[Triv -> triv]) `(,triv)]))

  (define (Pred prd)
    (match prd
      [(begin [Effect -> effect*] ... ,[pd])
       `(begin ,effect* ... ,pd)]
      [(if ,[pd0] ,[pd1] ,[pd2])
       `(if ,pd0 ,pd1 ,pd2)]
      [(,relop ,[Triv -> triv1] ,[Triv -> triv2])
       `(,relop ,triv1 ,triv2)]
      [,x x]))

  (define (Effect effect)
    (match effect
      [(set! ,[Var -> var] (,binop ,[Triv -> triv1] ,[Triv -> triv2]))
       `(set! ,var (,binop ,triv1 ,triv2))]
      [(set! ,[Var -> var] ,[Triv -> triv])
       `(set! ,var ,triv)]
      [(if ,[Pred -> prd] ,[effect1] ,[effect2])
       `(if ,prd ,effect1 ,effect2)]
      [(begin ,[effect*] ... [effect])
       `(begin ,effect* ... ,effect)]
      [(nop) `(nop)]))

  (define (Triv triv)  ; TODO: label
    (if (integer? triv)
        triv
        (Var triv)))

  (define (Var var)
    (cond
      [(frame-var->index var)
       => (lambda (idx) (make-disp-opnd 'rbp (* idx BYTES-OF-WORD)))]
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
    (if (jump? tail) tail`(,(add-dec! l tail))))

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
      [(nop) (lambda (tail) tail)]
      [,effect (lambda (tail) (begin-concat effect tail))]))

  (match pgm
    [(letrec ([,label* ,lmbd*] ...) ,tail) (skip-used-name label*)])

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
                    `(,@(Tail tail (caar decs))
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
   uncover-frame-conflict
   introduce-allocation-forms

   (iterate
    select-instructions
    uncover-register-conflict
    assign-registers
    (break-if everybody-home?)
    assign-frame
    finalize-frame-locations)

   discard-call-live
   finalize-locations
   expose-frame-var
   expose-basic-blocks
   flatten-program
   generate-x86-64
   ))

(define test1 '(letrec ()
                 (locals (a.1 b.2 c.3 d.4 e.5 x.6 y.7 z.8)
                         (begin
                           (set! r8 99)
                           (set! a.1 r8)
                           (set! b.2 fv0)
                           (set! c.3 (+ a.1 2))
                           (if (< c.3 0) (nop) (set! c.3 (+ c.3 b.2)))
                           (set! x.6 0)
                           (set! y.7 0)
                           (if (if (= c.3 1)
                                   (begin (set! e.5 0) (false))
                                   (begin (set! z.8 0) (= c.3 1)))
                               (set! d.4 y.7)
                               (set! d.4 x.6))
                           (set! rax (+ c.3 1))
                           (r15 rax rbp)))))

(define test2 '(letrec ([f$1 (lambda ()
                               (locals (x.1 y.2 z.3)
                                       (begin
                                         (set! x.1 1)
                                         (set! y.2 2)
                                         (set! z.3 3)
                                         (set! rax (+ x.1 y.2))
                                         (set! rax (+ x.1 z.3))
                                         (r15 rax rcx rdx rbx rbp rsi rdi r8 r9 r10
                                              r11 r12 r13 r14))))])
                 (locals () (f$1 rbp r15))))

(let ([pgm test2])
  (printf "source: ~n")
  (pretty-print pgm)
  (printf "~n~n")
  (let ([ret (with-output-to-file "_local_t.s"
               (lambda () (compiler pgm))
               'truncate)])
    (if (eq? ret (void))
        (void)
        (pretty-print ret))))

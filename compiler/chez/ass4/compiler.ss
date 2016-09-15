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

(define (binop? x)
  (memq x '(+ - * sra logand logor)))

(define (relop? x)
  (memq x '(= > < >= <=)))

;;;

(define (verify-scheme pgm) pgm)


(define (uncover-register-conflict pgm)

  (define (uncover tail)
    (define conflict '())
    (define (add-conflict var live*)
      (let ([ass (assq var conflict)])
        (if ass
            (set-cdr! ass (union live* (cdr ass)))
            (set! conflict (cons (cons var live*) conflict)))))

    (define (Triv triv)
      (if (or (uvar? triv) (register? triv) (frame-var? triv))
          `(,triv)
          '()))

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
         (union live* loc* triv-live*)]))

    (define (uncover-set var live* triv-live*)
      (let ([new-live* (difference live* `(,var))])
           (if (uvar? var)
               (add-conflict var new-live*)
               (void))
           (union new-live* triv-live*)))

    (define (check-live-set live*)
      (let ([uvar* (filter uvar? live*)])
        (unless (null? uvar*)
          (error 'check-live-set "uninitialized var" uvar*))))

    (begin
      (check-live-set (live-analysis tail '() #f))
      conflict))

  (define (Body body)
    (match body
      [(locals ,uvar* ,tail)
       `(locals ,uvar*
                (register-rconflict ,(uncover tail) ,tail))]))

  (match pgm
    [(letrec ([,label* (lambda () ,[Body -> body*])] ...)
       ,[Body -> body])
     `(letrec ([,label* (lambda () ,body*)] ...)
        ,body)]))


(define (assign-registers pgm)

  ; very simple assign
  (define (assign uvar* cgraph)
    (define (select-register regs conflicts)
      (if (null? regs)
          (error 'assign-registers "not enough registers")
          (if (memq (car regs) conflicts)
              (select-register (cdr regs) conflicts)
              (car regs))))
    (let loop ([cgraph cgraph] [res '()])
      (if (null? cgraph)
          (reverse res)
          (let ([uvar (caar cgraph)]
                [reg (select-register registers
                                      (map (lambda (v)
                                             (if (uvar? v)
                                                 (cadr (assq v res))
                                                 v))
                                           (cdar cgraph)))])
            (loop (cdr cgraph) (cons (list uvar reg) res))))))

  (define (Body body)
    (match body
      [(locals ,uvar* (register-rconflict ,conflict-graph ,tail))
       `(locate ,(assign uvar* conflict-graph) ,tail)]))

  (match pgm
    [(letrec ([,label* (lambda () ,[Body -> body*])] ...)
       ,[Body -> body])
     `(letrec ([,label* (lambda () ,body*)] ...)
        ,body)]))


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

  (define (finalize x env)
    (define (lookup v env)
      (let ([slot (assq v env)])
        ; if slot is #f ?
        (cdr slot)))
    (match x
      [(if ,[p] ,[e1] ,[e2]) `(if ,p ,e1 ,e2)]
      [(begin ,[ef*] ... ,[tail]) `(begin ,ef* ... ,tail)]
      [(set! ,[var] (,binop ,[triv1] ,[triv2]))
       `(set! ,var (,binop ,triv1 ,triv2))]
      [(set! ,[var] ,[triv]) `(set! ,var ,triv)]
      [(,relop ,[triv1] ,[triv2]) `(,relop ,triv1 ,triv2)]
      [,v (guard (uvar? v)) (lookup v env)]
      [,x x]))

  (define (Body body)
    (match body
      [(locate ([,uvar*, loc*] ...) ,tail)
       (finalize tail `((,uvar* . ,loc*) ...))]))

  (define (Dec dec)
    (match dec
      [(,label (lambda () ,[Body -> body]))
       `(,label (lambda () ,body))]))

  (match pgm
    [(letrec (,[Dec -> dec*] ...) ,[Body -> body])
     `(letrec ,dec* ,body)]))


(define (expose-frame-var pgm)

  (define (fvar->index fvar)
    (let ([fvar-str (symbol->string fvar)])
      (if (string=? (substring fvar-str 0 2) "fv")
          (string->number (substring fvar-str 2 (string-length fvar-str)))
          #f)))

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
      [(,relop ,[Triv -> triv1] [Triv -> triv2])
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

  (define (skip-used-label label*)
    (unless (null? label*)
      (let loop ([i (apply max (map (lambda (l)
                                      (string->number (extract-suffix l)))
                                    label*))])
        (unless (= i 0)
          (begin
            (unique-label 'skip)
            (loop (- i 1)))))))
  (match pgm
    [(letrec ([,label* ,lmbd*] ...) ,tail) (skip-used-label label*)])

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
        [(set! ,var ,triv) (emit 'movq triv var)]
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

(define (probe pass)
  (lambda (pgm)
    (let ([ret (pass pgm)])
      (if (eq? ret (void))
          (void)
          (pretty-print ret))
      ret)))

(define compiler
  (compiler-passes
   uncover-register-conflict
   assign-registers
   discard-call-live
   finalize-locations
   expose-frame-var
   expose-basic-blocks
   flatten-program
   generate-x86-64
   ))

(let ([pgm '(letrec ()
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
                        (r15 rax rbp))))])
  (printf "source: ~n")
  (pretty-print pgm)
  (printf "~n~n")
  (let ([ret (with-output-to-file "_local_t.s"
               (lambda () (compiler pgm))
               'truncate)])
    (if (eq? ret (void))
        (void)
        (pretty-print ret))))

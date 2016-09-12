; Program -> (letrec ([label (lambda () Tail)]*) Tail)
; Tail -> (Triv)
;       | (begin Effect* Tail)
; Effect -> (set! Var Triv)
;         | (set! Var (Binop Triv Triv))
; Var -> reg | fvar
; Triv -> Var | int | label
; reg -> rax | rcx | rdx | rbx |rbp | rsi | rdi
;      | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
; Binop -> + | - | * | logand | logor | sra
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

;;;

(define (verify-scheme pgm) pgm)

(define (expose-frame-var pgm)

  (define (fvar->index fvar)
    (let ([fvar-str (symbol->string fvar)])
      (if (string=? (substring fvar-str 0 2) "fv")
          (string->number (substring fvar-str 2 (string-length fvar-str)))
          #f)))

  (define (tail<= tail)
    (match tail
      [(begin ,effect* ... ,tl)
       `(begin ,@(map effect<= effect*) ,(tail<= tl))]
      [(,triv) `(,(triv<= triv))]))

  (define (effect<= effect)
    (match effect
      [(set! ,var (,binop ,triv1 ,triv2))
       `(set! ,(var<= var) (,binop ,(triv<= triv1) ,(triv<= triv2)))]
      [(set! ,var ,triv)
       `(set! ,(var<= var) ,(triv<= triv))]))

  (define (triv<= triv)  ; TODO: label
    (if (integer? triv)
        triv
        (var<= triv)))

  (define (var<= var)
    (cond
      [(frame-var->index var)
       => (lambda (idx) (make-disp-opnd 'rbp (* idx BYTES-OF-WORD)))]
      [else var]))

  (match pgm
    [(letrec (,dec* ...) ,tail)
     `(letrec ,(map (lambda (dec)
                      (match dec
                        [(,label (lambda () ,tl))
                         `(,label (lambda () ,(tail<= tl)))]))
                    dec*)
        ,(tail<= tail))]))

(define (flatten-program pgm)

  (define (tail<= tail)
    (match tail
      [(begin ,effect* ... ,tl)
       (append effect* (tail<= tl))]
      [(,triv) `((jump ,triv))]))

  (define (effect<= effect) effect)

  (match pgm
    [(letrec (,dec* ...) ,tail)
     `(code ,@(tail<= tail)
            ,@(fold-left (lambda (acc dec)
                           (append acc
                                   (match dec
                                     [(,label (lambda () ,tl))
                                      (cons label (tail<= tl))])))
                         '()
                         dec*))]))

; generator-x86-64

;[regs '(rax rcx rdx rbx rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)]

(define BYTES-OF-WORD 8)

(define (generate-x86-64 pgm)

  (define (stm<= stm)
    (define op-map '((+ . addq)
                     (- . subq)
                     (* . imulq)))
    (let* ([triv<= rand->x86-64-arg]
           [binop-lookup (lambda (binop)
                           (cond
                             [(assq binop op-map) => cdr]
                             [else (error 'binop-lookup
                                          "unsupported binop: ~a" binop)]))])
      (match stm
        [(set! ,var (,binop ,var ,triv))
         (emit (binop-lookup binop) triv var)]
        [(set! ,var ,triv) (emit 'movq triv var)]
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

(define compiler
  (compiler-passes
   expose-frame-var
   flatten-program
   generate-x86-64
   ))

(let ([pgm '(letrec ([f$1 (lambda ()
                            (begin
                              (set! fv0 rax)
                              (set! rax (+ rax rax))
                              (set! rax (+ rax fv0))
                              (r15)))])
              (begin
                (set! rax 17)
                (f$1)))])
  (let ([ret (with-output-to-file "_local_t.s"
               (lambda () (compiler pgm))
               'truncate)])
    (if (eq? ret (void))
        (void)
        (pretty-print ret))))

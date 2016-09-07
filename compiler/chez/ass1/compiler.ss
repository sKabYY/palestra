; Program -> (begin Statement+)
; Statement -> (set! Var1 int64)
;            | (set! Var1 Var2)
;            | (set! Var1 (Binop Var1 int32))
;            | (set! Var1 (Binop Var1 Var2))
; Var -> rax | rcx | rdx | rbx |rbp | rsi | rdi
;      | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
; Binop -> + | - | *
;

(case-sensitive #t)
(load "../lib/match.ss")

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

(define (verify-scheme program) program)

; generator-x86-64

(define (assemble-template code)
  (format ".global _scheme_entry
_scheme_entry:
~aret
" code))

;[regs '(rax rcx rdx rbx rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)]

(define (stm-gen stm)
  (define op-map '((+ . addq)
                   (- . subq)
                   (* . imulq)))
  (let* ([emit (lambda (inst o1 o2) (format "~a ~a, ~a" inst o1 o2))]
         [int-gen (lambda (i) (format "$~a" i))]
         [var-gen (lambda (var) (format "%~a" var))]
         [var-or-int-gen (lambda (vi) (if (integer? vi)
                                          (int-gen vi)
                                          (var-gen vi)))]
         [binop-lookup (lambda (binop)
                         (cond
                           [(assq binop op-map) => cdr]
                           [else (error 'binop-lookup
                                        "unsupported binop: ~a" binop)]))])
    (match stm
      [(set! ,var1 (,binop ,var1 ,var-or-int32))
       (emit (binop-lookup binop)
             (var-or-int-gen var-or-int32)
             (var-gen var1))]
      [(set! ,var1 ,var-or-int64)
       (emit 'movq
             (var-or-int-gen var-or-int64)
             (var-gen var1))])))

(define (generate-x86-64 program)
  (match program
    [(begin ,stm+ ...)
     (assemble-template
      (apply string-append
             (map (lambda (stm) (string-append "    " (stm-gen stm) "\n"))
                  stm+)))]))

(let ([pgm '(begin
              (set! rax 8)
              (set! rcx 3)
              (set! rax (- rax rcx)))])
  (printf "~a" (generate-x86-64 pgm)))

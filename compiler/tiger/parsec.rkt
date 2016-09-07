#lang racket

(provide (combine-out debug!
                      set-delims!
                      set-line-comment!
                      set-comment-start!
                      set-comment-end!
                      set-operators!
                      set-quotation-marks!
                      set-lisp-char!
                      set-significant-whitespaces!
                      set-literal-types!
                      scan
                      @... @= @or @* @+ @? @.@ @glob
                      $empty $pred $_ $token-type
                      :: ::=
                      parse-tokens
                      parse
                      node->list))

(require "utils.rkt")
(require "structs.rkt")
(require "error.rkt")
;-------------------------------------------------------------
;                        utilities
;-------------------------------------------------------------

(define dump (lambda ss (void)))
(define debug dump)
(define (debug?) (not (eq? debug dump)))
(define (debug!)
  (set! debug
        (lambda ss
          (printf "[debug] ")
          (apply put ss)
          (printf "~n~n"))))


(define char->string string)


(define (fatal . args)
  (apply throw error:parse args))


(define (tok-pos-info toks)
  (define *max-length* 15)
  (let loop ([toks toks] [i 0] [acc '()])
    (let ([tok (car toks)])
      (cond
        [(eof-token? tok) (if (null? acc)
                              'EOF
                              (reverse acc))]
        [(>= i *max-length*) (reverse (cons '... acc))]
        [else (loop (cdr toks)
                    (add1 i)
                    (cons (token-text tok) acc))]))))


;; s-expression settings
;; please override for other languages.
(define *delims* (list "("  ")"  "["  "]"  "{"  "}" "'"  "`"  ","))
(define *line-comment* (list ";"))
(define *comment-start* "")
(define *comment-end* "")
(define *operators*  '())
(define *quotation-marks* '(#\" #\'))
(define *lisp-char* (list "#\\" "?\\"))
(define *significant-whitespaces* '())
(define *literal-types* '())


(define (set-delims! x)
  (set! *delims* x))

(define (set-line-comment! x)
  (set! *line-comment* x))

(define (set-comment-start! x)
  (set! *comment-start* x))

(define (set-comment-end! x)
  (set! *comment-end* x))

(define (set-operators! x)
  (set! *operators* x))

(define (set-quotation-marks! x)
  (set! *quotation-marks* x))

(define (set-lisp-char! x)
  (set! *lisp-char* x))

(define (set-significant-whitespaces! x)
  (set! *significant-whitespaces* x))

(define-syntax-rule (set-literal-types! lts ...)
  (do-set-literal-types! (lts->list lts ...)))
(define (do-set-literal-types! lts)
  (set! *literal-types* lts))
(define-syntax lts->list
  (syntax-rules ()
    [(_) '()]
    [(_ (name pred?) lts ...)
     (cons (cons name pred?) (lts->list lts ...))]))


;-------------------------------------------------------------
;                          scanner
;-------------------------------------------------------------
;; intermediate data for scanner
(struct VE (val end) #:transparent)
(struct Token (type text start end) #:transparent)
(struct eof-token (pos) #:transparent)

(define (token-start t)
  (cond
    [(Token? t) (Token-start t)]
    [(eof-token? t) (eof-token-pos t)]))
(define token-type Token-type)
(define token-text Token-text)

(define (token-type-eq? tok type)
  (and (Token? tok)
       (symbol=? (token-type tok) type)))

(define (comment? tok) (token-type-eq? tok 'comment))


(define whitespace?  char-whitespace?)


; Is char c a delimeter?
(define (delim? c)
  (member (char->string c) *delims*))


(define (start-with s start prefix)
  (let* ([prefix-str (if (char? prefix)
                         (char->string prefix)
                         prefix)]
         [len (string-length prefix-str)]
         [end (+ start len)])
    (cond
      [(= len 0) #f]
      [(< (string-length s) end) #f]
      [(string=? (substring s start end) prefix-str)
       (VE prefix-str end)]
      [else #f])))



(define (start-with-one-of s start prefixes)
  (cond
    [(null? prefixes) #f]
    [(start-with s start (car prefixes)) =>
     (lambda (ve) ve)]
    [else
     (start-with-one-of s start (cdr prefixes))]))

; (start-with-one-of "+>>=" 0 (list ">" #\+))


(define (find-next s start pred?)
  (cond
    [(<= (string-length s) start) #f]
    [(pred? s start) start]
    [else
     (find-next s (add1 start) pred?)]))



; Find the first delim that match the start of s
(define (find-delim s start)
  (start-with-one-of s start *delims*))


(define (find-operator s start)
  (start-with-one-of s start *operators*))

; (find-operator ">> x" 0)


(define (find-literal-type text)
  (let loop ([ts *literal-types*])
    (cond
      [(null? ts) #f]
      [((cdar ts) text) (caar ts)]
      [else (loop (cdr ts))])))


(define (scan s)
  (define (scan1 s start)
    (let ([ve->values
           (lambda (type)
             (lambda (ve)
               (values (Token type (VE-val ve) start (VE-end ve))
                       (VE-end ve))))])
      (cond
        [(= start (string-length s))
         (values (eof-token start) start)]

        [(start-with-one-of s start *significant-whitespaces*) =>
         (ve->values 'newline)]

        [(whitespace? (string-ref s start))
         (scan1 s (add1 start))]

        ; line comment
        [(start-with-one-of s start *line-comment*)
         (let ([line-end (or (find-next s start
                                        (lambda (s start)
                                          (char=? (string-ref s start) #\newline)))
                              (string-length s))])
           (values (Token 'comment
                          (substring s start line-end)
                          start
                          (add1 line-end))
                   line-end))]

        ; block comment
        [(start-with s start *comment-start*)
         (let ([end (cond
                      [(find-next s start
                                  (lambda (s start)
                                    (start-with s start *comment-end*))) =>
                       (lambda (pos) (+ pos (string-length *comment-end*)))]
                      [else (string-length s)])])
           (values (Token 'comment
                          (substring s start end)
                          start
                          end)
                   end))]

        [(find-delim s start) =>
         (ve->values 'token)]

        [(find-operator s start) =>
         (ve->values 'operator)]

        ; string
        [(start-with-one-of s start *quotation-marks*) =>
         (lambda (ve)
           (let* ([qm (VE-val ve)]
                  [reg-match (regexp-match
                              (regexp (string-replace "^{}(\\\\.|[^{}])*{}"
                                                      "{}"
                                                      qm))
                              s start)])
             (cond
               [(not reg-match)
                (fatal "scan error: string match error")]
               [else
                (let ([end (+ start (string-length (car reg-match)))])
                  (values (Token 'str (car reg-match) start end)
                          end))])))]

        ; scheme/elisp char
        [(start-with-one-of s start *lisp-char*) =>
         (lambda (ve)
           (cond
             [(<= (string-length s) (VE-end ve))
              (error "scan error: reached EOF while scanning char")]
             [else
              (let ([end
                     (let loop ([end (add1 (VE-end ve))])
                       (cond
                         [(or (<= (string-length s) end)
                              (whitespace? (string-ref s end))
                              (find-delim s end))
                          end]
                         [else (loop (add1 end))]))])
                (values (Token 'char (string-ref s (sub1 end)) start end)
                        end))]))]

        ; identifier or literal type
        [else
         (let loop ([pos start] [chars '()])
           (cond
             [(or (<= (string-length s) pos)
                  (whitespace? (string-ref s pos))
                  (find-delim s pos)
                  (find-operator s pos))
              (let ([text (list->string (reverse chars))])
                (values (Token (or (find-literal-type text) 'token)
                               text
                               start
                               pos)
                        pos))]
             [else
              (loop (add1 pos) (cons (string-ref s pos) chars))]))])))

  (let loop ([start 0] [toks '()])
    (let-values ([(tok newstart) (scan1 s start)])
      (let ([newtoks (cons tok toks)])
        (cond
          [(eof-token? tok)
           (reverse newtoks)]
          [else
           (loop newstart newtoks)])))))


;-------------------------------------------------------------
;                           parser
;-------------------------------------------------------------

; stack for checking left recursion
(define (empty-stack) '())

(define (extend-stack stk p t)
  (cons (cons p t) stk))

(define (on-stack? stk p t)
  (cond
    [(null? stk) #f]
    [(and (eq? p (caar stk))
          (eq? t (cdar stk)))
     stk]
    [else (on-stack? (cdr stk) p t)]))

(define (stack->string stk)
  (let ([ps (map (lambda (x) (format "~a" (car x)))
                 stk)])
    (string-join ps "\n")))


(define (apply-check parser toks stk)
  (cond
    [(on-stack? stk parser toks) =>
     (lambda (t)
       (fatal "left-recursion detected\n"
              "parser: " parser "\n"
              "start token: " (car toks) "\n"
              "stack trace: " (stack->string t)))]
    [else ((parser) toks (extend-stack stk parser toks))]))


(define (append-nodes nodes)
  (apply append
         (map (lambda (n) (if (list? n) n (list n)))
              nodes)))


; toks always has a eof token at last

(define (comb ps)
  (lambda ()
    (lambda (toks stk)
      (let loop ([ps ps] [toks toks] [acc '()])
        (if (null? ps)
            (values acc toks)
            (let-values ([(t r) (apply-check (car ps) toks stk)])
              (if t
                  (loop (cdr ps) r (append acc t))
                  (values #f r))))))))


(define (@... . ps) (comb ps))


(define (@= type . ps)
  (let ([parser (comb ps)])
    (lambda ()
      (lambda (toks stk)
        (debug "test node: "
               type
               "~n" "        "
               (tok-pos-info toks))
        (let-values ([(t r) ((parser) toks stk)])
          (if t
              (values (list (new-node type
                                      t
                                      (token-start (car toks))
                                      (token-start (car r))))
                      r)
              (values #f r)))))))


(define (@or . ps)
  (lambda ()
    (lambda (toks stk)
      (let loop ([ps ps] [min-r #f])
        (if (null? ps)
            (values #f (if min-r min-r toks))
            (let-values ([(t r) (apply-check (car ps) toks stk)])
              (if t
                  (values t r)
                  (loop (cdr ps)
                        (if (and min-r (< (length min-r) (length r)))
                            min-r
                            r)))))))))


(define (@* . ps)
  (let ([parser (comb ps)])
    (lambda ()
      (lambda (toks stk)
        (let loop ([toks toks] [acc '()])
          (let-values ([(t r) ((parser) toks stk)])
            (if t
                (loop r (append acc t))
                (values acc toks))))))))


(define (@+ . ps)
  (let ([parser (comb ps)])
    (@... parser (@* parser))))


(define (@? . ps) (@or (comb ps) $empty))


(define (@.@ . ps)
  (let ([rps (reverse ps)])
    (let ([sep (car rps)]
          [parser (comb (reverse (cdr rps)))])
      (@or (@... parser (@* sep parser))
           $empty))))


(define (@glob . ps)
  (let ([parser (comb ps)])
    (lambda ()
      (lambda (toks stk)
        (let-values ([(t r) ((parser) toks stk)])
          (if t
              (values '() r)
              (values #f r)))))))


(define $empty
  (lambda ()
    (lambda (toks stk) (values '() toks))))


(define ($pred pred?)
  (lambda ()
    (lambda (toks stk)
      (let* ([tok (car toks)])
        (if (eof-token? tok)
            (values #f toks)
            (let ([type [token-type tok]]
                  [text (token-text tok)])
              (if (pred? type text)
                  (values (list text) (cdr toks))
                  (values #f toks))))))))


(define ($_ s)
  (@glob ($pred (lambda (type text)
                  (debug "test token: \"" s "\" [text=\"" text "\"]")
                  (and
                   (symbol=? type 'token)
                   (string=? text s))))))


(define ($token-type tp)
  ($pred (lambda (type text)
           (symbol=? type tp))))

(define (@cache parser)
  (let ([cache (make-hash)])
    (lambda ()
      (lambda (toks stk)
        (if (and (debug?) (hash-has-key? cache toks))
            (debug "hit cache: "
                   parser
                   "~n" "        "
                   (tok-pos-info toks))
            (void))
        (let ([p (hash-ref! cache toks
                            (lambda ()
                              (let-values ([(t r) ((parser) toks stk)])
                                (cons t r))))])
          (values (car p) (cdr p)))))))


(define-syntax-rule (::cache name expr)
  (define name (@cache (lambda () (expr)))))


(define-syntax-rule (:: name ps ...)
  (::cache name (@... ps ...)))


(define-syntax-rule (::= name type ps ...)
  (::cache name (@= type ps ...)))


(define (parse-tokens parser toks)
  (let-values ([(t r) ((parser) toks (empty-stack))])
    (if (and t (eof-token? (car r)) (null? (cdr r)))
        t
        (fatal "syntax error at: " (tok-pos-info r)))))


(define (parse parser src)
  (parse-tokens parser
                (filter (lambda (tok) (not (comment? tok)))
                        (scan src))))

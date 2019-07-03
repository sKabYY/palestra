(load "mklib.ss")

(define-syntax noto
  (syntax-rules ()
    [(_ g* ...)
     (conda
       [g* *u] ...
       [*s])]))

(define-syntax distincto
  (syntax-rules ()
    [(_ v1 v2) (noto (== v1 v2))]
    [(_ v1 v2 v* ...)
     (all
       (noto (== v1 v2) (== v1 v*) ...)
       (distincto v2 v* ...))]))

(define words
  '((a s t a n t e)
    (a s t o r i a)
    (b a r a t t o)
    (c o b a l t o)
    (p i s t o l a)
    (s t a t a l e)))

;        v1  v2  v3
;    ---------------
;    |   a   b   s
; h1 | a s t a n t e
;    |   t   r   a
; h2 | c o b a l t o
;    |   r   t   a
; h3 | p i s t o l a
;    |   a   o   e

(define res
  (run* (v1 v2 v3 h1 h2 h3)
    (fresh (v11 v12 v13 v14 v15 v16 v17
            v21 v22 v23 v24 v25 v26 v27
            v31 v32 v33 v34 v35 v36 v37
            h11 h12 h13 h14 h15 h16 h17
            h21 h22 h23 h24 h25 h26 h27
            h31 h32 h33 h34 h35 h36 h37)
      (== v1 (list v11 v12 v13 v14 v15 v16 v17))
      (== v2 (list v21 v22 v23 v24 v25 v26 v27))
      (== v3 (list v31 v32 v33 v34 v35 v36 v37))
      (== h1 (list h11 h12 h13 h14 h15 h16 h17))
      (== h2 (list h21 h22 h23 h24 h25 h26 h27))
      (== h3 (list h31 h32 h33 h34 h35 h36 h37))
      (== h12 v12) (== h14 v22) (== h16 v32)
      (== h22 v14) (== h24 v24) (== h26 v34)
      (== h32 v16) (== h34 v26) (== h36 v36)
      (membero v1 words)
      (membero v2 words)
      (membero v3 words)
      (membero h1 words)
      (membero h2 words)
      (membero h3 words)
      (distincto v1 v2 v3 h1 h2 h3))))

(define (parse-world chars)
  (string->symbol
    (apply string-append (map symbol->string chars))))

(pretty-print
  (map (lambda (r) (map parse-world r)) res))
#lang racket
(provide [combine-out cps-v0
                      cps-v0*
                      cps-v1
                      cps-v2
                      cps-v2*])

; Expression = Variable
;            | (lambda (Variable) Expression)
;            | (Expression Expression)
;
; CPS:
; ---
; SpExpr = Variable
;        | (lambda (Variable) (lambda (Variable) TfExpr))
; TfExpr = ((SpExpr Cont) SpExpr)
;        | (Cont SpExpr)
; Cont = (lambda (Variable) TfExpr)

(define (mkvar s)
  (string->symbol (string-append "#" s)))

(define (cps-v0 expr)
  (letrec ((vk (mkvar "k"))
           (fv (let ((n 0))
                 (lambda ()
                   (set! n (+ n 1))
                   (mkvar (number->string n)))))
           (end-cont `(lambda (v) v))
           (>>
            (lambda (expr cont)
              (match expr
                [(? symbol? s) `(,cont ,s)]
                [`(lambda (,a) ,body)
                 `(,cont (lambda (,vk) (lambda (,a) ,(>> body vk))))]
                [`(,rator ,rand)
                 (let ((v1 (fv))
                       (v2 (fv)))
                   (>> rator `(lambda (,v1)
                                ,(>> rand `(lambda (,v2)
                                             ((,v1 ,cont) ,v2))))))]))))
    (>> expr end-cont)))

(define (cps-v0* expr)
  (letrec ((vk (mkvar "k"))
           (fv (let ((n 0))
                 (lambda ()
                   (set! n (+ n 1))
                   (mkvar (number->string n)))))
           (>>
            (lambda (expr)
              (match expr
                [(? symbol? s) `(lambda (,vk) (,vk ,s))]
                [`(lambda (,a) ,body)
                 `(lambda (,vk) (,vk (lambda (,a) ,(>> body))))]
                [`(,e1 ,e2)
                 (let ((v1 (fv))
                       (v2 (fv)))
                   `(lambda (,vk)
                      (,(>> e1) (lambda (,v1)
                                  (,(>> e2) (lambda (,v2)
                                              ((,v1 ,v2) ,vk)))))))]))))
    (>> expr)))

(define (cps-v1 expr)
  (letrec ((vk (mkvar "k"))
           (eta-cont (lambda (v) `(,vk ,v)))
           (fv (let ((n 0))
                 (lambda ()
                   (set! n (+ n 1))
                   (mkvar (number->string n)))))
           (end-cont (lambda (v) v))
           (>>
            (lambda (expr cont)
              (match expr
                [(? symbol? s) (cont s)]
                [`(lambda (,a) ,body)
                 (cont `(lambda (,vk) (lambda (,a) ,(>> body eta-cont))))]
                [`(,e1 ,e2)
                 (let ((v0 (fv)))
                   (>> e1 (lambda (v1)
                            (>> e2 (lambda (v2)
                                     `((,v1 (lambda (,v0) ,(cont v0))) ,v2))))))]))))
    (>> expr end-cont)))

(define (cps-v2 expr)
  (letrec ((vk (mkvar "k"))
           (eta-cont (lambda (v) `(,vk ,v)))  ; tail context
           (fv (let ((n 0))
                 (lambda ()
                   (set! n (+ n 1))
                   (mkvar (number->string n)))))
           (end-cont (lambda (v) v))
           (>>
            (lambda (expr cont)
              (match expr
                [(? symbol? s) (cont s)]
                [`(lambda (,a) ,body)
                 (cont `(lambda (,vk) (lambda (,a) ,(>> body eta-cont))))]
                [`(,e1 ,e2)
                 (match e1
                   ;[`(lambda (,a) ,body)  ; <-- This may be wrong.
                   ; (>> e2 (lambda (v2)
                   ;          `((lambda (,a) ,(>> body cont)) ,v2)))]
                   [else
                    (>> e1
                        (lambda (v1)
                          (>> e2
                              (lambda (v2)
                                (if (eq? cont eta-cont)
                                    `((,v1 ,vk) ,v2)
                                    (let ((v0 (fv)))
                                      `((,v1 (lambda (,v0) ,(cont v0))) ,v2)))))))])]))))
    (>> expr end-cont)))

(define (cps-v2* expr)
  (letrec ((vk (mkvar "k"))
           (fv (let ((n 0))
                 (lambda ()
                   (set! n (+ n 1))
                   (mkvar (number->string n)))))
           (tail-cont (lambda (v) `(,vk ,v)))
           (>>
            (lambda (expr)
              (match expr
                [(? symbol? s) (lambda (k) (k s))]
                [`(lambda (,a) ,body)
                 (lambda (k) (k `(lambda (,a) (lambda (,vk) ,((>> body) tail-cont)))))]
                [`(,e1 ,e2)
                 (let ((v0 (fv)))
                   (lambda (k)
                     ((>> e1) (lambda (v1)
                                ((>> e2) (lambda (v2)
                                           `((,v1 ,v2)
                                             ,(if (eq? k tail-cont)
                                                  vk
                                                  `(lambda (,v0) ,(k v0))))))))))]))))
    ((>> expr) (lambda (e) `(lambda (,vk) (,vk ,e))))))

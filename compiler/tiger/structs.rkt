#lang racket

(provide (combine-out new-node
                      node->list
                      nmatch))

(require "utils.rkt")


(struct Node (type elts start end [exdat #:mutable])
  #:transparent)

(define (new-node type elts start end)
  (Node type elts start end #f))

(define (node->list n)
  (cond
    [(Node? n) (list (Node-type n) (node->list (Node-elts n)))]
    [(list? n)
     (cons '=
           (let loop ([lst n])
             (if (null? lst)
                 '()
                 (cons (node->list (car lst)) (loop (cdr lst))))))]
    [else n]))

(define-syntax nmatch
  ; (assert (list? (Node-elts node)))
  (syntax-rules (else)
    [(_ node)
     (begin
       (put "not pattern matched: " node "~n")
       (error 'not-matched))]
    [(_ node (else body ...))
     (begin body ...)]
    [(_ node [type (pat ...) body ...] cases ...)
     (if (symbol=? type (Node-type node))
         (match (Node-elts node)
           [(list pat ...) body ...]
           [else
            (put "match-error:" "~n"
                 "  pattern: " '(pat ...) "~n"
                 "  content: " (Node-elts node) "~n")
            (error 'match-error)])
         (nmatch node cases ...))]
    [(_ node [type val body ...] cases ...)
     (if (symbol=? type (Node-type node))
         (let ([val (Node-elts node)]) body ...)
         (nmatch node cases ...))]))

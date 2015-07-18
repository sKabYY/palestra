#lang eopl

(#%require "lib.rkt")

(define-datatype bintree bintree?
  (leaf-node
    (num integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)))

(define (leaf? tree)
  (cases bintree tree
    (leaf-node (num) #t)
    (else #f)))

(define (max-interior tree)
  (define (one-leaf-pair key num subtree)
    (let ((pair (max-pair subtree)))
      (if (>= num 0)
        (cons key (+ num (cdr pair)))
        pair)))
  (define (max-pair tree)
    (cases bintree tree
      (leaf-node (num) '*never-reach-here*)
      (interior-node (key left right)
        (cases bintree left
          (leaf-node (lnum)
            (cases bintree right
              (leaf-node (rnum)
                (cons key (+ lnum rnum)))
              (interior-node (rkey rleft rright)
                (one-leaf-pair key lnum right))))
          (interior-node (lkey lleft lright)
            (cases bintree right
              (leaf-node (rnum)
                (one-leaf-pair key rnum left))
              (interior-node (rkey rleft rright)
                (let ((lpair (max-pair left))
                      (rpair (max-pair right)))
                  (let ((mpair (if (>= (cdr lpair) (cdr rpair))
                                 lpair
                                 rpair))
                        (sum (+ (cdr lpair) (cdr rpair))))
                    (if (>= sum (cdr mpair))
                      (cons key sum)
                      mpair))))))))))
  (cases bintree tree
    (leaf-node (num)
      (eopl:error "Cannot apply max-interior on leaf-node:" tree))
    (interior-node (key left right)
      (car (max-pair tree)))))

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))
(run-disp
  (max-interior tree-2)
  (max-interior tree-3))
(max-interior (leaf-node 0))

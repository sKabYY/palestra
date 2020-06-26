(load "mk.ss")
(load "mklib.ss")

(define-syntax dummy
  (syntax-rules ()
    [(_ e) (void)]))

(define (:: x type) (membero x type))
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

(define man '(George John Rebert))
(define woman '(Barbara Christine Yolanda))
(define person (append man woman))

(define-syntax uniq-ppl
  (syntax-rules ()
    [(_ p* ...)
     (all
       (:: p* person) ...
       (distincto p* ...))]))

(define res
  (run 10
       (Bathroom Dining Kitchen Livingroom Pantry Study
        Bag Firearm Gas Knife Poison Rope
        X)
    (:: X person)
    (:: Bathroom person)
    (:: Dining person)
    (:: Kitchen person)
    ; clue 5
    (conde
      [(== Livingroom 'John)]
      [(== Livingroom 'George)])
    ;;;
    (:: Livingroom person)
    (:: Pantry person)
    (:: Study person)
    ; clue 2
    (conde
      [(== Bathroom 'Barbara) (== Study 'Yolanda)]
      [(== Bathroom 'Yolanda) (== Study 'Barbara)])
    ;;;
    (distincto Bathroom Dining Kitchen Livingroom Pantry Study)
    (:: Bag person)
    (:: Firearm person)
    ; clue 8
    (== Firearm 'George)
    ;;;
    (:: Gas person)
    (:: Knife person)
    (:: Poison person)
    (:: Rope person)
    (distincto Bag Firearm Gas Knife Poison Rope)
    ; clue 1
    (:: Kitchen man)
    (noto (== Kitchen Rope) (== Kitchen Knife) (== Kitchen Bag) (== Kitchen Firearm))
    ;;;
    ; clue 3
    (noto (== Bag 'Barbara) (== Bag 'George))
    (noto (== Bag Bathroom) (== Bag Dining))
    ;;;
    ; clue 4
    (:: Rope woman)
    (== Rope Study)
    ;;;
    ; clue 6
    (noto (== Knife Dining))
    ;;;
    ; clue 7
    (noto (== Study 'Yolanda) (== Pantry 'Yolanda))
    ;;;
    ; clue 9
    (== X Pantry)
    (== X Gas)
    ;;;
    ))

(pretty-print
  (map (lambda (r)
         (map list '(Bathroom Dining Kitchen Livingroom Pantry Study Bag Firearm Gas Knife Poison Rope X)
                    r))
       res))

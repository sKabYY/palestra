#lang racket

(require "parsec.rkt")

(provide (combine-out $program))

(define delims (list "(" ")" "[" "]" "{" "}" ";" ","))
(set-delims! delims)
(set-line-comment! (list "//"))
(set-comment-start! "/*")
(set-comment-end! "*/")
(set-operators! '())
(set-quotation-marks! '())
(set-lisp-char! '())

(define true-literal "true")
(define false-literal "false")
(set-literal-types!
 ('integer (lambda (s)
             (cond
               [(string->number s) =>
                (lambda (n) (integer? n))]
               [else #f])))
 ('boolean (lambda (s)
             (or (string=? s true-literal)
                 (string=? s false-literal)))))

(:: |(| ($_ "("))
(:: |)| ($_ ")"))
(:: |[| ($_ "["))
(:: |]| ($_ "]"))
(:: |{| ($_ "{"))
(:: |}| ($_ "}"))
(:: |;| ($_ ";"))
(:: |,| ($_ ","))
(:: |=| ($_ "="))

(::= $program 'program
     (@or $main  ; reduce trying times
          (@... (@* $function-definition) $main)))

(:: $main ($_ "main") $statement)

(::= $function-definition 'fundef
     $return-type $identifier
     (@= 'parameters |(| (@.@ $type $identifier |,|) |)|)
     $statement)

(:: $statement
    (@or (@= 'block |{| (@* $statement) |}|)
         (@= 'empty |;|)
         (@= 'vardef  ($_ "var") $identifier |=| $expression |;|)
         (@= 'if ($_ "if") $expression $statement (@? ($_ "else") $statement))
         (@= 'while ($_ "while") $expression $statement)
         (@= 'return ($_ "return") (@? $expression) |;|)
         (@= 'output |(| ($_ "output") $expression |)| |;|)
         (@= 'call |(| (@+ $expression) |)| |;|)
         (@= 'assignment $identifier |=| $expression |;|)
         (@= 'arrayset $arrayref-expression |=| $expression |;|)))

(:: $expression
    (@or $arrayref-expression
         $simple-expression))

(::= $arrayref-expression 'arrayref
    $simple-expression (@+ |[| $expression |]|))

(:: $simple-expression
    (@or (@= 'literal $literal)
         (@= 'mkarray $type |[| $expression |]|)
         (@= 'andop ($_ "and") |(| $expression |,| $expression |)|)
         (@= 'orop ($_ "or") |(| $expression |,| $expression |)|)
         (@= 'opd $identifier |(|
             (@= 'arguments (@.@ $expression |,|)) |)|)
         $identifier
         (@= 'application |(| (@+ $expression) |)|)))

(:: $literal
    (@or (@= 'integer-literal ($token-type 'integer))
         (@= 'boolean-literal ($token-type 'boolean))))

(:: $return-type
    (@or (@= 'void-type ($_ "void"))
         $type))

(:: $type
    (@or (@= 'integer-type ($_ "int"))
         (@= 'boolean-type ($_ "bool"))))

(::= $identifier 'id
     ($pred (lambda (type text)
              (and (symbol=? type 'token)
                   (not (memq text delims))))))

#lang eopl

(#%provide (all-defined))

(define let-cases
  (list

"42"
"+(1, 2)"
"-(1, 2)"
"*(2, 3)"
"quotient(5, 2)"
"remainder(12, 7)"
"minus(2)"
"zero?(0)"
"zero?(1)"
"equal?(1, 1)"
"equal?(1, 2)"
"greater?(1, 2)"
"greater?(2, 2)"
"greater?(3, 2)"
"less?(1, 2)"
"less?(2, 2)"
"less?(3, 2)"
"list(1, 2, 3)"
"cons(0, cons(1, emptylist))"
"cons(0, list(1, 2, 3))"
"car(list(1, 2, 3))"
"cdr(list(1, 2, 3))"
"null?(list(1, 2))"
"null?(emptylist)"
"if zero?(0) then +(1, 1) else -(1, 1)"
"if zero?(1) then +(1, 1) else -(1, 1)"
"let x = 12144 in x"
"let x = 12, y = 3 in +(x, y)"
"let x = 12 in let x = 13 in *(x, 2)"

))

(define proc-cases
  (list

"(proc (x, y) +(x, y) 2 3)"

"let f = proc (x) -(x, 11)
in (f (f 77))"

"(proc (f) (f (f 77))
proc (x) -(x, 11))"

"let x = 200
in let f = proc (z) -(z, x)
   in let x = 100
      in let g = proc (z) -(z, x)
         in -((f 1), (g 1))"

"let makemult = proc (maker)
                proc (x)
                 if zero?(x)
                 then 0
                 else +(4, ((maker maker) -(x, 1)))
in let times4 = proc (x) ((makemult makemult) x)
   in (times4 3)"

"let addx = proc (y) proc (x) +(x, y)
in let add1 = (addx 1)
   in (add1 12)"

"let Y = proc (f)
         (proc (u) (u u)
          proc (x)
           (f proc (v) ((x x) v)))
in let double = (Y proc (d)
                    proc (x)
                     if zero?(x)
                     then 0
                     else +(2, (d -(x, 1))))
   in (double 12)"

))

(define letrec-cases
  (list

"letrec double (x) = if zero?(x) then 0 else +(2, (double -(x, 1)))
in (double 12)"

"letrec
  even(x) = if zero?(x) then 1 else (odd -(x, 1)),
  odd(x) = if zero?(x) then 0 else (even -(x, 1))
in (odd 13)"

"letrec gcd(a, b) = if zero?(a) then b else (gcd remainder(b, a) a)
in (gcd 144 12144)"

))

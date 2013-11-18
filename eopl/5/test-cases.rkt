#lang eopl

(#%provide (all-defined))

(define let-cases
  (list

"42" 42
"+(1, 2)" 3
"-(1, 2)" -1
"*(2, 3)" 6
"quotient(5, 2)" 2
"remainder(12, 7)" 5
"minus(2)" -2
"zero?(0)" #t
"zero?(1)" #f
"equal?(1, 1)" #t
"equal?(1, 2)" #f
"greater?(1, 2)" #f
"greater?(2, 2)" #f
"greater?(3, 2)" #t
"less?(1, 2)" #t
"less?(2, 2)" #f
"less?(3, 2)" #f
"if zero?(0) then +(1, 1) else -(1, 1)" 2
"if zero?(1) then +(1, 1) else -(1, 1)" 0
"let x = 12144 in x" 12144
"let x = 12, y = 3 in -(x, y)" 9
"let x = 12 in let x = 13 in *(x, 2)" 26

))

(define proc-cases1
  (list

"(proc (x, y) +(x, y) 2 3)" 5

"let f = proc (x) -(x, 11)
in (f (f 77))" 55

"(proc (f) (f (f 77))
proc (x) -(x, 11))" 55

"let x = 200
in let f = proc (z) -(z, x)
   in let x = 100
      in let g = proc (z) -(z, x)
         in -((f 1), (g 1))" -100

"let makemult = proc (maker)
                proc (x)
                 if zero?(x)
                 then 0
                 else +(4, ((maker maker) -(x, 1)))
in let times4 = proc (x) ((makemult makemult) x)
   in (times4 3)" 12

"let addx = proc (y) proc (x) +(x, y)
in let add1 = (addx 1)
   in (add1 12)" 13

"let Y = proc (f)
         (proc (u) (u u)
          proc (mk)
           (f proc (v) ((mk mk) v)))
in let double = (Y proc (d)
                    proc (x)
                     if zero?(x)
                     then 0
                     else +(2, (d -(x, 1))))
   in (double 12)" 24

"let Y = proc (f)
         (proc (u) (u u)
          proc (mk)
           (f proc (v1, v2) ((mk mk) v1 v2)))
in let gcd = (Y proc (gcd0)
                 proc (a, b)
                  if zero?(a)
                  then b
                  else (gcd0 remainder(b, a) a))
   in (gcd 144 12144)" 48

))
(define proc-cases (append let-cases proc-cases1))

(define letrec-cases1
  (list

"letrec double (x) = if zero?(x) then 0 else +(2, (double -(x, 1)))
in (double 12)" 24

"letrec
  even(x) = if zero?(x) then 1 else (odd -(x, 1)),
  odd(x) = if zero?(x) then 0 else (even -(x, 1))
in (odd 13)" 1

"letrec gcd(a, b) = if zero?(a) then b else (gcd remainder(b, a) a)
in (gcd 144 12144)" 48

))
(define letrec-cases (append proc-cases letrec-cases1))

(define imprefs-cases1
  (list

"let i = 1 in set i = 2" '**void**
"let i = 1 in begin set i = 2, i end" 2

"let new_counter = proc ()
                   let counter = 0
                   in proc ()
                       begin
                        set counter = +(counter, 1),
                        counter
                       end
in let c1 = (new_counter), c2 = (new_counter)
   in begin (c1), (c1), (c2), *((c1), (c2)) end" 6

"let p = pair(11, 12) in *(left(p), right(p))" 132

"let p = pair(11, 12)
in begin setleft(p, 9), setright(p, 8), *(left(p), right(p)) end" 72

"let glo = pair(11, 22)
in let f = proc (p)
            begin setright(p, left(p)), setleft(p, 99), -(left(p), right(p)) end
   in (f glo)" 88

"let p = proc (x) set x = 4
in let a = 3
   in begin (p a), a end" 3

"let f = proc (x, y) -(x, y)
in (f 88 99)" -11

"let x = 1
in let p = ref x
   in begin setref(p, 0), x end" 0

"let swap = proc (a, b)
            let tmp = deref(a)
            in begin setref(a, deref(b)), setref(b, tmp) end
in let x = 10, y = 20
   in begin (swap ref x ref y), -(x, y) end" 10

))
(define imprefs-cases (append letrec-cases imprefs-cases1))

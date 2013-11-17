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
"if zero?(0) then +(1, 1) else -(1, 1)"
"if zero?(1) then +(1, 1) else -(1, 1)"
"let x = 12144 in x"
"let x = 12, y = 3 in +(x, y)"
"let x = 12 in let x = 13 in *(x, 2)"

))

(define proc-cases1
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
          proc (mk)
           (f proc (v) ((mk mk) v)))
in let double = (Y proc (d)
                    proc (x)
                     if zero?(x)
                     then 0
                     else +(2, (d -(x, 1))))
   in (double 12)"

"let Y = proc (f)
         (proc (u) (u u)
          proc (mk)
           (f proc (v1, v2) ((mk mk) v1 v2)))
in let gcd = (Y proc (gcd0)
                 proc (a, b)
                  if zero?(a)
                  then b
                  else (gcd0 remainder(b, a) a))
   in (gcd 144 12144)"

))
(define proc-cases (append let-cases proc-cases1))

(define letrec-cases1
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
(define letrec-cases (append proc-cases letrec-cases1))

(define exprefs-cases1
  (list

"let g = newref(11) in deref(g)"
"let g = newref(11) in setref(g, 12)"
"let g = newref(11) in begin setref(g, 12), deref(g) end"
"let g = newref(newref(11)) in deref(deref(g))"

"let new_counter = proc ()
                   let counter = newref(0)
                   in proc ()
                       begin
                        setref(counter, +(1, deref(counter))),
                        deref(counter)
                       end
in let c1 = (new_counter), c2 = (new_counter)
   in begin (c1), (c1), (c2), *((c1), (c2)) end"  ; 3 * 2 = 6

))
(define exprefs-cases (append letrec-cases exprefs-cases1))

(define imprefs-cases1
  (list

"let i = 1 in set i = 2"
"let i = 1 in begin set i = 2, i end"

"let new_counter = proc ()
                   let counter = 0
                   in proc ()
                       begin
                        set counter = +(counter, 1),
                        counter
                       end
in let c1 = (new_counter), c2 = (new_counter)
   in begin (c1), (c1), (c2), *((c1), (c2)) end"  ; 3 * 2 = 6

"let p = pair(11, 12) in *(left(p), right(p))"

"let p = pair(11, 12)
in begin setleft(p, 9), setright(p, 8), *(left(p), right(p)) end"

"let glo = pair(11, 22)
in let f = proc (p)
            begin setright(p, left(p)), setleft(p, 99), -(left(p), right(p)) end
   in (f glo)"

"let p = proc (x) set x = 4
in let a = 3
   in begin (p a), a end"

"let swap = proc (a, b)
            let tmp = a
            in begin set a = b, set b = tmp end
in let x = 10, y = 20
   in begin (swap ref x ref y), -(x, y) end"

"let swap = proc (a, b)
            let tmp = a
            in begin set a = b, set b = tmp end
in let x = 10, y = 20
   in begin (swap x y), -(x, y) end"

))
(define imprefs-cases (append letrec-cases imprefs-cases1))

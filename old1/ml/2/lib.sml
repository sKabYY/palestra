val pi = 3.14159;

fun area(r) = pi * r * r;

fun increase(k, n) =
  if (k+1)*(k+1) > n then k else k+1;
fun introot(n) =
  if n=0 then 0 else increase(2 * introot(n div 4), n);

fun iseven(n) = n mod 2 = 0;

fun gcd(a, b) =
  if a=b then a
  else if iseven(a) then
    if iseven(b) then 2*gcd(a div 2, b div 2)
    else gcd(a div 2, b)
  else
    if iseven(b) then gcd(a, b div 2)
    else gcd(abs(b div 2 - a div 2), a);

signature ARITH =
sig
  type t
  val zero : t
  val sum : t * t -> t
  val diff : t * t -> t
  val prod : t * t -> t
  val quo : t * t -> t
end;

structure Complex : ARITH =
struct
  type t = real*real;
  val zero = (0.0, 0.0);
  fun sum((x,y), (x',y')) = (x+x', y+y') : t;
  fun diff((x,y), (x',y')) = (x-x', y-y') : t;
  fun prod((x,y), (x',y')) = (x*x' - y*y', x*y' + x'*y) : t;
  fun recip(x,y) = let val t = x*x + y*y
                   in (x/t, ~y/t) end;
  fun quo(z, z') = prod(z, recip z');
end;

structure Real : ARITH =
struct
  type t = real;
  val zero = 0.0;
  fun sum(x, x') = x + x' : t;
  fun diff(x, x') = x - x' : t;
  fun prod(x, x') = x * x' : t;
  fun quo(x, x') = x / x' : t;
end;

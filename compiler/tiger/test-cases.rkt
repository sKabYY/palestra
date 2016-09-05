#lang racket
(require "error.rkt")

(provide (all-defined-out))

(define *exit-at-exception* #f)
(define (exit-at-exception!) (set! *exit-at-exception* #t))
(define *color-on* #t)
(define (color code) (if *color-on* (display code) (void)))
(define (begin-red) (color "\033[91m"))
(define (begin-green) (color "\033[92m"))
(define (begin-yellow) (color "\033[93m"))
(define (end-color) (color "\033[0m"))

(define (test interp cases)
  (define (iter cases total wrong)
    (if (null? cases)
        (printf "SUMMARY(wrong/total): ~a/~a.~%" wrong total)
        (let ((src (car cases))
              (ans (cadr cases))
              (rest (cddr cases)))
          (begin-green)
          (displayln src)
          (begin-red)
          (let ((correct (with-handlers
                           [(error:base?
                             (lambda (e)
                               (displayln (error:base-message e))
                               (if *exit-at-exception*
                                   (raise e)
                                   (and (procedure? ans)
                                        (eq? #t (ans e))))))]
                           (let ((ret (interp src)))
                             (display ">> ")
                             (pretty-print ret)
                             (equal? ret ans)))))
            (begin-yellow)
            (if correct
                (void)
                (printf "[WRONG!]ans=~a~%" ans))
            (end-color)
            (newline)
            (iter rest (+ total 1) (if correct wrong (+ wrong 1)))))))
  (iter cases 0 0))


(define cases
  `(

"main {
  (output 42);
}" 42

"main {
  (output true);
}" #t

"main {
  (output false);
}" #f

"main {
  var i = 42;
  (output i);
}" 42

"main {
  var b = true;
  (output b);
}" #t

"main {
  (output int[3]);
}" ,(make-vector 3 0)

"main {
  (output int[0]);
}" ,error:mk-array-invalid-length?

"main {
  (output int[-1]);
}" ,error:mk-array-invalid-length?

"main {
  var arr = int[3];
  (output arr[0]);
}" 0

"main {
  var arr = int[3];
  (output arr[1]);
}" 0

"main {
  var arr = int[3];
  (output arr[2]);
}" 0

"main {
  var arr = int[3];
  (output arr[-1]);
}" ,error:index-out-of-range?

"main {
  var arr = int[3];
  (output arr[3]);
}" ,error:index-out-of-range?

"main {
  var arr = int[3];
  arr[1] = 2;
  (output arr[1]);
}" 2

"main {
  var arr = int[3];
  arr[-(0, 0)] = 2;
  (output arr[1]);
}" 0

"main {
  var arr = int[3];
  arr[2] = +(1, 2);
  (output arr[0]);
}" 0

"main {
  (output arrlen(int[+(1, 2)]));
}" 3

"main {
  var arr = int[3];
  (output arrlen(arr));
}" 3

"main {(output +(12, 34));}" 46
"main {(output -(34, 12));}" 22
"main {(output *(12, 11));}" 132
"main {(output div(13, 3));}" 4
"main {(output mod(13, 3));}" 1
"main {(output =(32, 32));}" #t
"main {(output =(32, 21));}" #f
"main {(output >(32, 31));}" #t
"main {(output >(32, 32));}" #f
"main {(output >(31, 32));}" #f
"main {(output <(31, 32));}" #t
"main {(output <(31, 31));}" #f
"main {(output <(32, 31));}" #f
"main {(output >=(32, 32));}" #t
"main {(output >=(33, 32));}" #t
"main {(output >=(33, 34));}" #f
"main {(output <=(31, 32));}" #t
"main {(output <=(32, 32));}" #t
"main {(output <=(32, 31));}" #f
"main {(output +(+(1, 2), 3));}" 6

"main {(output not(true));}" #f
"main {(output not(false));}" #t
"main {(output not(=(31, 31)));}" #f
"main {(output not(=(32, 31)));}" #t

"main {
  if true (output 11); else (output 22);
}" 11
"main {
  if false (output 11); else (output 22);
}" 22
"main {
  if true {
    var a = 1;
    (output a);
  } else {
    var a = 2;
    (output a);
  }
}" 1

"main {
  var i = +(4, 2);  // comment
  i = +(1, /*xxx*/-2);
  (output i);
}" -1

"main {
  var i = +(4, 2);
  i = +(1, 2);
  (output i);
}" 3

"main {
  var sum = 0;
  var i = 0;
  while <(i, 3) {
    i = +(i, 1);
    sum = +(sum, i);
  }
  (output sum);
}" 6

"main {
  var x = 33;
  var i = 0;
  while <(i, 3) {
    i = +(i, 1);
    var x = i;
  }
  (output x);
}" 33

"main {
  var sum = 0;
  var i = 0;
  while <(i, 100) {
    i = +(i, 1);
    sum = +(sum, i);
  }
  (output sum);
}" 5050

"void f() (output 42);
main {
  (f);
}" 42

"int f(int x, int y) return +(x, y);
main {
  var i = (f 12 32);
  (output i);
}" 44

"int f(int x, int y) return +(x, y);
int g(int x) return +(11, x);
main {
  var i = (g (f 12 32));
  (output i);
}" 55

"int gcd(int a, int b) {
  if =(a, 0)
    return b;
  else
    return (gcd mod(b, a) a);
}
main {
  var i = (gcd 144 12144);
  (output i);
}" 48

"int gcd(int a, int b) {
  while not(=(a, 0)) {
    var t = a;
    a = mod(b, a);
    b = t;
  }
  return b;
}
main {
  var i = (gcd 144 12144);
  (output i);
}" 48

"bool even?(int n) {
  if =(n, 0)
    return true;
  else
    return (odd? -(n, 1));
}
bool odd?(int n) {
  if =(n, 0)
    return false;
  else
    return (even? -(n, 1));
}
main {
  var b = (even? 42);
  (output b);
}" #t

"int f() { return 1; return 2; }
main (output (f));" 1

"main return 1;" ,error:return-outside-function?

"main (output i);" ,error:unbound?

"main {
  var i = 0;
  i = true;
}" ,error:type-checking?

"bool f(bool b) return b;
main (f 1);" ,error:type-checking?

"int f(int i) return i;
main {
  if (f 1) (output 1); else (output 2);
}" ,error:type-checking?

"main (output +(1, true));" ,error:type-checking?
"main (output not(1));" ,error:type-checking?

"int f(int x) return x;
main (f 1 2);" ,error:argsnum-not-match?

"void f() (output i);
main {
  var i = 0;
  (f);;
}" ,error:unbound?

"int f() (output 0);
main (f);" ,error:type-checking?

"void f() return 1;
main (f);" ,error:type-checking?

"int f() return true;
main (f);" ,error:type-checking?

"main {(output +(12, 34);}" ,error:parse?

"int gcd(int a, int b) {
  while not(=(a, 0)) {
    var t = a;
    a = mod(b, a);
    b = t;
  }
  return b;
}" ,error:parse?

))

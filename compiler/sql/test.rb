#!/usr/bin/env ruby

require 'pp'

module TestScanner

  require './scanner'

  s = <<EOF
  ; a'bc'd
aa(cc"kk\\"kk"("ss")aa;c
/*te\nst*/;ac
EOF
  p s
  scanner = CCParsec::Scanner.new
  scanner.comment_start = '/*'
  scanner.comment_end = '*/'
  stream = scanner.scan(s)
  puts stream.to_list
  stream = stream.filter_comment
  puts stream.to_list

end

puts

module TestXCcc

  require './xccc'

  str = IO.read('sexp.grammer')
  ast = CCParsec::xccc_grammer.parse(str)
  if ast.success?
    pp ast
  else
    puts "#{ast.message} at #{ast.rest.inspect}"
    pp ast.rest.car
  end

end

puts

module TestXCcc

  require './xccc'

  puts "start #{self}"

  str = IO.read('sexp.grammer')
  grammer = CCParsec::xccc_define_grammer(str) do
    p 'init'
  end
  #sexp_code = '(x y)'
  sexp_code = <<EOF
(define (double x) (+ x x))
(define (gcd a b) (if (= a 0) b (gcd (remainder b a) a)))
EOF
  ast = grammer.parse(sexp_code)
  if ast.success?
    pp ast.to_sexp
  else
    pp ast
  end

end

puts

require './otest'

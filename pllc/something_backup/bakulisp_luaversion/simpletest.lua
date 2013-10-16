#!/usr/bin/env lua

require('bakulib')

-- main test ----------------------------------------------

function print_result(r)
	if type(r) == 'table' then
		assert(r.t == 'CLOSURE', 'unknown type: ' .. tostring(r.t))
		print('env:')
		print_env(r.env)
		print('agrument: "' .. r.a .. '"')
		print('body:')
		print_ast(r.b)
	else
		print(tostring(r))
	end
end

--[[
source = [[
(
	(define mod11 (\x (% x 11)))
	(mod11
		((\x (* 7 x))
			((\x\y (+ x y)) 3 2)))
)
]]
source = [[
(
	(define gcd (
		\a\b (
			if (= b 0) a (gcd b (% a b))
		)))
	(gcd 1024 56)
)
]]
--[[
source = [[
(
	(define add1 (+ 1))
	(define add2 (\x (add1 (add1 x))))
	(add1 (add2 2))
)
]]
print('source: \n' .. source)
lex_tree = build_lex_tree(source)
print()
print_lex_tree(lex_tree)
ast = build_ast(lex_tree)
print()
print_ast(ast)
result = interp(ast)
print()
print(source)
print('=')
print_result(result)

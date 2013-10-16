#!/usr/bin/env lua
-- Lua 5.2.2

-- stack --------------------------------------------------
function stack_clone(src, start)
	if start == nil then
		start = 1
	end
	dst = {}
	for i = start, #src do
		dst[i - start + 1] = src[i]
	end
	return dst
end

function top(stack)
	return stack[#stack]
end

function push(stack, elem)
	table.insert(stack, elem)
end

function pop(stack)
	assert(#stack > 0)
	elem = stack[#stack]
	stack[#stack] = nil
	return elem
end
-----------------------------------------------------------

-- symbols ------------------------------------------------
local DEFINE = 'define'
local IF = 'if'
local LPAREN = '%('
local RPAREN = '%)'

local LAMBDA = '\\'

local PLUS = '%+'
local MINUS = '%-'
local MULTI = '%*'
local DIV = '/'
local MOD = '%%'

local EQ = '='

local SYMBOL = '[_a-zA-Z][_a-zA-Z0-9]*'
local NUMBER = '[%+%-]?%d+%.?%d*'

local NEWLINE = '\n'
local SPACE = '%s+'

local WORDS = {
	{DEFINE},
	{IF},
	{LPAREN},
	{RPAREN},
	{LAMBDA},
	{PLUS, SYMBOL},
	{MINUS, SYMBOL},
	{MULTI, SYMBOL},
	{DIV, SYMBOL},
	{MOD, SYMBOL},
	{EQ, SYMBOL},
	{SYMBOL},
	{NUMBER},
	{NEWLINE},
	{SPACE},
}

-- ast symbols
local AST_DEFINE = 'AST_DEFINE'
local AST_IF = 'AST_IF'
local AST_SYMBOL = 'AST_SYMBOL'
local AST_NUMBER = 'AST_NUMBER'
local AST_BUILDIN = 'AST_BUILDIN'
local AST_LAMBDA = 'AST_LAMBDA'
local AST_APPLICATION = 'AST_APPLICATION'
-----------------------------------------------------------

-- scanner ------------------------------------------------
function scanner1(text, start)
	for _, pattern in ipairs(WORDS) do
		p = '^' .. pattern[1]
		s, e = string.find(text, p, start)
		if s ~= nil then
			local rpattern
			if #pattern == 1 then
				rpattern = pattern[1]
			else
				rpattern = pattern[2]
			end
			return rpattern, string.sub(text, s, e), e + 1
		end
	end
end

function new_scanner(text)
	local n = 1
	return function()
		if n ~= nil then
			pattern, str, n = scanner1(text, n)
			return pattern, str
		end
	end
end
-----------------------------------------------------------

-- functions for printing ---------------------------------
function print_indent(indent)
	for _ = 1, indent do
		io.write('  ')
	end
end

function dump_table(t)
	print(tostring(t))
	for k, v in pairs(t) do
		print_indent(1)
		print(tostring(k) .. tostring(v))
	end
end

function print_node(indent, node)
	print_indent(indent)
	if node.t ~= nil then
		print(node.v .. ': ' .. node.t .. ' <> ' .. node.lineno)
	else
		print('(#' .. #node .. ')')
		for _, n in ipairs(node) do
			print_node(indent + 1, n)
		end
	end
end

function print_lex_tree(tree)
	for _, n in ipairs(tree) do
		print_node(0, n)
	end
end

function print_ast1(indent, node)
	print_indent(indent)
	local d = {
		[AST_SYMBOL] = function()
			print(AST_SYMBOL .. ': ' .. node.v)
		end,
		[AST_NUMBER] = function()
			print(AST_NUMBER .. ': ' .. node.v)
		end,
		[AST_DEFINE] = function()
			print(AST_DEFINE)
			print_indent(indent)
			print('name')
			print_ast1(indent + 1, node.s)
			print_indent(indent)
			print('value')
			print_ast1(indent + 1, node.v)
		end,
		[AST_IF] = function()
			print(AST_IF)
			print_indent(indent)
			print('condition')
			print_ast1(indent + 1, node.c)
			print_indent(indent)
			print('then')
			print_ast1(indent + 1, node.a)
			print_indent(indent)
			print('else')
			print_ast1(indent + 1, node.b)
		end,
		[AST_LAMBDA] = function()
			print(AST_LAMBDA)
			print_indent(indent)
			print('argument')
			print_ast1(indent + 1, node.a)
			print_indent(indent)
			print('body')
			print_ast1(indent + 1, node.b)
		end,
		[AST_APPLICATION] = function()
			print(AST_APPLICATION)
			print_indent(indent)
			print('function')
			print_ast1(indent + 1, node.f)
			print_indent(indent)
			print('argument')
			print_ast1(indent + 1, node.a)
		end,
		[AST_BUILDIN] = function()
			print(AST_BUILDIN .. node.name .. '(argc=' .. #node.as .. ')')
		end
	}
	d[node.t]()
end

function print_ast(ast)
	print_ast1(0, ast)
end

local UP_ENV_IDX
function print_env(env)
	cur_env = env
	repeat
		for k, v in pairs(cur_env) do
			if type(k) ~= 'number' then
				print(k .. ': ' .. tostring(v))
			end
		end
		cur_env = cur_env[UP_ENV_IDX]  -- TODO
	until cur_env == nil
end
-----------------------------------------------------------

-- parse --------------------------------------------------
-- type of node of lex_tree:
--   all except LPAREN, RPAREN, NEWLINE, SPACE
--   struct node: {
--     t: type,  // t=nil for (...)
--     v: value,
--     lineno: line number,
--   }
function build_lex_tree(text)
	local scanner = new_scanner(text)
	local stack = {}
	local lex_tree = {}
	local node = lex_tree
	local lineno = 1
	for p, s in scanner do
		if p == LPAREN then
			local child = {}
			push(node, child)
			push(stack, node)
			node = child
		elseif p == RPAREN then
			node = pop(stack)
		elseif p == NEWLINE then
			lineno = lineno + 1
		elseif p ~= SPACE then  -- ignore SPACE
			table.insert(node, {t=p, v=s, lineno=lineno})
		end
	end
	return lex_tree
end

-- build AST
-- type of ast_node:
--   symbol (node.t == SYMBOL)
--   number (node.t == NUMBER)
--   build-in {t, name, op, as}
--   define {t, s, v}
--   lambda {t, a, b}
--   application {t, f, a}
function ast_symbol(v)
	return {t = AST_SYMBOL, v = v}
end
function ast_number(v)
	return {t = AST_NUMBER, v = v}
end
function parse_ast_node(node)
	if node.t ~= nil then
		-- node is an element
		local parse_elem = {
			[SYMBOL] = function()
				return ast_symbol(node.v)
			end,
			[NUMBER] = function()
				return ast_number(tonumber(node.v))
			end,
		}
		return parse_elem[node.t]()
	elseif #node == 1 then
		-- node is a list which contains only one element
		return parse_ast_node(node[1])
	else
		-- startswith DEFINE: define
		-- startswith IF: if
		-- startswith LPAREN: lambda
		-- else: application
		local first_type = node[1].t
		if first_type == DEFINE then
			-- define
			assert(#node == 3)
			return {
				t = AST_DEFINE,
				s = parse_ast_node(node[2]),  -- symbol
				v = parse_ast_node(node[3]),  -- value
			}
		elseif first_type == IF then
			-- if
			-- #node == 4: if cond then else
			assert(#node == 4)
			return {
				t = AST_IF,
				c = parse_ast_node(node[2]),  -- cond
				a = parse_ast_node(node[3]),  -- then
				b = parse_ast_node(node[4]),  -- else
			}
		elseif first_type == LAMBDA then
			-- lambda
			assert(#node >= 3)
			return {
				t = AST_LAMBDA,
				a = parse_ast_node(node[2]),  -- argument
				b = parse_ast_node(stack_clone(node, 3)),  -- body
			}
		else
			-- application
			function parse_application(node, e)
				if e == 1 then
					return parse_ast_node(node[e])
				else
					return {
						t = AST_APPLICATION,
						f = parse_application(node, e - 1),  -- function
						a = parse_ast_node(node[e]),  -- argument
					}
				end
			end
			return parse_application(node, #node)
		end
	end
end

function build_ast(lex_tree)
	return parse_ast_node(lex_tree)
end
-----------------------------------------------------------

-- build-in functions -------------------------------------
-- struct build-in: {t, name, op, as}

local INTP_DEFINE = 'DEFINE'
function new_define(env)
	return {
		t = INTP_DEFINE,
		env = env,
	}
end

local INTP_CLOSURE = 'CLOSURE'
function new_closure(argument, body, env)
	return {
		t = INTP_CLOSURE,
		a = argument,
		b = body,
		env = env,
	}
end

function BNA(i)  -- short for buildin argument
	return '#' .. tostring(i)
end

function lambda_to_closure(lambda, env)
	return new_closure(lambda.a.v, lambda.b, env)
end

function buildin_to_closure(name, op, argc, env)
	local as = {}
	for i = 1, argc do
		push(as, ast_symbol(BNA(i)))
	end
	local ast_buildin = {
		t = AST_BUILDIN,
		name = name,
		op = op,
		as = as,
	}
	local tmp_as = stack_clone(as)
	local body = ast_buildin
	while #tmp_as ~= 0 do
		a = pop(tmp_as)
		body = {
			t = AST_LAMBDA,
			a = a,
			b = body,
		}
	end
	return lambda_to_closure(body, env)
end

function buildin_number_add(env)
	local op = function(as) return as[1] + as[2] end
	return buildin_to_closure('#add', op, 2, env)
end

function buildin_number_minus(env)
	local op = function(as) return as[1] - as[2] end
	return buildin_to_closure('#minus', op, 2, env)
end

function buildin_number_multi(env)
	local op = function(as) return as[1] * as[2] end
	return buildin_to_closure('#multi', op, 2, env)
end

function buildin_number_div(env)
	local op = function(as) return as[1] / as[2] end
	return buildin_to_closure('#div', op, 2, env)
end

function buildin_number_mod(env)
	local op = function(as) return as[1] % as[2] end
	return buildin_to_closure('#mod', op, 2, env)
end

function buildin_eq(env)
	local op = function(as) return as[1] == as[2] end
	return buildin_to_closure('#eq', op, 2, env)
end

-----------------------------------------------------------

-- interp -------------------------------------------------

local UP_ENV_IDX = 0

function lookup(x, env)
	local cur_env = env
	repeat
		v = cur_env[x]
		if v == nil then
			cur_env = cur_env[UP_ENV_IDX]
		else
			return v
		end
	until cur_env == nil
end

function ext_env(env, key, value)
	assert(type(key) == 'string', 'key type error: ' .. type(key))
	return {
		[key] = value,
		[UP_ENV_IDX] = env,
	}
end

function global_env()
	local operators = {
		['+'] = buildin_number_add,
		['-'] = buildin_number_minus,
		['*'] = buildin_number_multi,
		['/'] = buildin_number_div,
		['%'] = buildin_number_mod,
		['='] = buildin_eq,
	}
	local env = {}
	for o, mk in pairs(operators) do
		env[o] = mk(env)
	end
	return env
end

function interp1(ast_node, env)
	local d = {
		[AST_SYMBOL] = function()
			local value = lookup(ast_node.v, env)
			assert(value ~= nil,
					'Symbol ' .. ast_node.v .. ' not found!')
			return value
		end,
		[AST_NUMBER] = function()
			return ast_node.v
		end,
		[AST_BUILDIN] = function()
			local args = {}
			for _, a in ipairs(ast_node.as) do
				push(args, interp1(a, env))
			end
			return ast_node.op(args)
		end,
		[AST_DEFINE] = function()
			assert(ast_node.s.t == AST_SYMBOL)
			local name = ast_node.s.v
			local value = interp1(ast_node.v, env)
			local new_env = ext_env(env, name, value)
			if value.t == INTP_CLOSURE then
				value.env = ext_env(value.env, name, value)
			end
			return new_define(new_env)
		end,
		[AST_IF] = function()
			local value = interp1(ast_node.c, env)
			if value then
				return interp1(ast_node.a, env)
			else
				return interp1(ast_node.b, env)
			end
		end,
		[AST_LAMBDA] = function()
			return lambda_to_closure(ast_node, env)
		end,
		[AST_APPLICATION] = function()
			local closure = interp1(ast_node.f, env)
			if closure.t == INTP_CLOSURE then
				local a = interp1(ast_node.a, env)
				local new_env = ext_env(closure.env, closure.a, a)
				return interp1(closure.b, new_env)
			elseif closure.t == INTP_DEFINE then
				return interp1(ast_node.a, closure.env)
			else
				assert(false, 'application: unknown type: ' .. closure.t)
			end
		end,
	}
	return d[ast_node.t]()
end

function interp(ast)
	env = global_env()
	return interp1(ast, env)
end
-----------------------------------------------------------

-- dostring -----------------------------------------------
function bakulisp(text)
	lex_tree = build_lex_tree(text)
	ast = build_ast(lex_tree)
	result = interp(ast)
	return result
end
-----------------------------------------------------------

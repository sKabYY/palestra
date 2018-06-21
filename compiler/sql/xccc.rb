# x compiler combinator compiler

require './parsec'
require './match'

module Parsec

  _xccc_grammer = define_grammer do

    set_delims ['(', ')', '[', ']', '{', '}', '`', ',', '::', '=', ':', '.']
    set_quotation_marks ['\'']
    set_regex_marks ['/']

    lparen = pEq('(')
    rparen = pEq(')')
    word = pTokenType(:str)
    regex = pTokenType(:regex)
    var = cIfFail('invalid var', pRegex(/^[[:alnum:]_-]+$/))
    cplus = pEq('@+')
    cplus = pEq('@*')
    cor = pEq('@or')
    cseq = pEq('@..')
    cnot = pEq('@!')
    cmaybe = pEq('@?')
    cmb_op = cIfFail('unknown op',
                     cOr(cIs(:plus_cmb, pEq('@+')),
                         cIs(:star_cmb, pEq('@*')),
                         cIs(:or_cmb, pEq('@or')),
                         cIs(:seq_cmb, pEq('@..')),
                         cIs(:not_cmb, pEq('@!')),
                         cIs(:maybe_cmb, pEq('@?'))))

    named_exp = cSeq(var, pEq(':'), get(:exp))
    op_exp = cSeq(lparen, cmb_op, cStar(get(:exp)), rparen)
    seq_exp = cSeq(lparen, cPlus(get(:exp)), rparen)
    exp = def_parser(:exp, cOr(cIs(:named_exp, named_exp),
                               cIs(:op_exp, op_exp),
                               cIs(:seq_exp, seq_exp),
                               cIs(:word_exp, word),
                               cIs(:regex_exp, regex),
                               cIs(:var_exp, var)))

    def_stm = cIs(:def_stm, cSeq(var, pEq('='), cPlus(exp), pEq('.')))
    root_stm = cIs(:def_root, cSeq(pEq('::'), var))

    return_parser cIs(:program, cSeq(root_stm, cPlus(def_stm)))

  end

  define_method :xccc_grammer do
    _xccc_grammer
  end

  def xccc_ast_to_grammer(ast, &block)
    define_grammer do

      unless block.nil?
        self.instance_eval(&block)
      end

      nodes = ast.children
      root_stm = nodes.first
      def_stms = nodes[1..-1]
      root_name = root_stm.children.first.value.to_sym

      self.class.send :define_method, :apply_op do |op, ps|
        match op, self do
          type :plus_cmb do || cPlus(*ps) end
          type :star_cmb do || cStar(*ps) end
          type :or_cmb do cOr(*ps) end
          type :seq_cmb do cSeq(*ps) end
          type :not_cmb do cNot(*ps) end
          type :maybe_cmb do cMaybe(*ps) end
        end
      end

      self.class.send :define_method, :value_of do |exp|
        match exp, self do
          type :named_exp do |var, e|
            name = var.value.to_sym
            cIs(name, value_of(e))
          end
          type :op_exp do |op, *es|
            ps = es.map { |e| value_of(e) }
            apply_op(op, ps)
          end
          type :seq_exp do |*es|
            ps = es.map { |e| value_of(e) }
            cSeq(*ps)
          end
          type :word_exp do |word|
            pEq(word.value, false)  # TODO
          end
          type :regex_exp do |regex|
            pRegex(regex.value)
          end
          type :var_exp do |var|
            name = var.value.to_sym
            get(name)
          end
        end
      end

      def_stms.each do |def_stm|
        match def_stm, self do
          type :def_stm do |var, *es|
            name = var.value.to_sym
            ps = es.map { |e| value_of(e) }
            def_parser(name, cSeq(*ps))
          end
        end
      end

      return_parser get(root_name)

    end
  end


  def xccc_define_grammer(str, &block)
    ast = xccc_grammer.parse(str)
    unless ast.success?
      linenum, colnum = offset_to_position(str, ast.fail_rest.car.start_idx)
      raise "(#{linenum}, #{colnum}){ message: #{ast.message}, next_tok: #{ast.fail_rest.car.inspect} }"
    end
    xccc_ast_to_grammer(ast.nodes.first, &block)
  end

  module_function :xccc_grammer
  module_function :xccc_ast_to_grammer
  module_function :xccc_define_grammer

end

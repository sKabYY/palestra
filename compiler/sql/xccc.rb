# x compiler combinator compiler

require './parsec'
require './match'

module Parsec

  _xccc_grammer = define_grammer do

    set_delims ['(', ')', '[', ']', '{', '}', '`', ',', '::', '=', ':']
    set_quotation_marks ['\'']

    lparen = pEq('(')
    rparen = pEq(')')
    word = pTokenType(:str)
    var = pTokenPred do |type, text|
      !!text.match(/^[[:alnum:]]+$/)
    end
    cplus = pEq('@+')
    cor = pEq('@or')
    cseq = pEq('@..')
    cnot = pEq('@!')

    named_exp = cSeq(var, pEq(':'), get(:exp))
    plus_exp = cSeq(lparen, cplus, cPlus(get(:exp)), rparen)
    or_exp = cSeq(lparen, cor, cPlus(get(:exp)), rparen)
    seq_exp = cSeq(lparen, cseq, cPlus(get(:exp)), rparen)
    not_exp = cSeq(lparen, cnot, cPlus(get(:exp)), rparen)
    exp = def_parser(:exp, cOr(cIs(:named_exp, named_exp),
                               cIs(:plus_exp, plus_exp),
                               cIs(:or_exp, or_exp),
                               cIs(:seq_exp, seq_exp),
                               cIs(:not_exp, not_exp),
                               cIs(:var_exp, var),
                               cIs(:word_exp, word)))

    def_stm = cIs(:def_stm, cSeq(var, pEq('='), exp))
    root_stm = cIs(:def_root, cSeq(pEq('::'), var))

    return_parser cIs(:program, cSeq(root_stm, cPlus(def_stm)))

  end

  define_method :xccc_grammer do
    _xccc_grammer
  end

  def xccc_ast_to_grammer(ast)
    define_grammer do

      nodes = ast.children
      root_stm = nodes.first
      def_stms = nodes[1..-1]
      root_name = root_stm.children.first.value.to_sym

      self.class.send :define_method, :value_of do |exp|
        match exp, self do
          type :named_exp do |var, e|
            name = var.value.to_sym
            cIs(name, value_of(e))
          end
          type :plus_exp do |*es|
            ps = es.map { |e| value_of(e) }
            cPlus(*ps)
          end
          type :or_exp do |*es|
            ps = es.map { |e| value_of(e) }
            cOr(*ps)
          end
          type :seq_exp do |*es|
            ps = es.map { |e| value_of(e) }
            cSeq(*ps)
          end
          type :not_exp do |*es|
            ps = es.map { |e| value_of(e) }
            cNot(*ps)
          end
          type :var_exp do |var|
            name = var.value.to_sym
            get(name)
          end
          type :word_exp do |word|
            pEq(word.value)
          end
        end
      end

      def_stms.each do |def_stm|
        match def_stm, self do
          type :def_stm do |var, exp|
            name = var.value.to_sym
            def_parser(name, value_of(exp))
          end
        end
      end

      return_parser get(root_name)

    end
  end


  def xccc_define_grammer(str)
    ast = xccc_grammer.parse(str)
    raise ast unless ast.success?
    xccc_ast_to_grammer(ast.nodes.first)
  end


  module_function :xccc_grammer
  module_function :xccc_ast_to_grammer
  module_function :xccc_define_grammer

end

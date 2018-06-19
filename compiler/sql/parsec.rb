require './scanner'

module Parsec

  class Parsec

    attr_reader :scanner, :root_parser

    def initialize
      @scanner = Scanner.new
      @root_parser = nil
      @env = {}
    end

    def return_parser(parser)
      @root_parser = parser
    end

    def set_delims(delims)
      @scanner.delims = delims
    end

    def set_quotation_marks(qms)
      @scanner.quotation_marks = qms
    end

    def def_parser(name, parser)
      _env_set(name, parser)
    end

    def get(name)
      ParserImpl.new do |toks, stk|
        parser = _env_get(name)
        parser.parse(toks, stk)
      end
    end

    def cIs(type, *ps)
      parser = cSeq(*ps)
      ParserImpl.new do |toks, stk|
        r = parser.parse(toks, stk)
        next r unless r.success?
        _output_node(Node.make_node(type, r.nodes),
                     r.rest)
      end
    end

    def pTokenPred(fail_message='PRED fail')
      ParserImpl.new do |toks, stk|
        tok = toks.car
        next _output_fail("reach eof", toks) if tok.eof?
        if yield(tok.type, tok.text)
          _output_node(Node.make_leaf(tok), toks.cdr)
        else
          _output_fail(fail_message, toks)
        end
      end
    end

    def pEq(s, case_sensitive=true)
      p = pTokenPred "expect \"#{s}\"" do |type, text|
        if case_sensitive
          text == s
        else
          text.downcase == s.downcase
        end
      end
      cGlob(p)
    end

    def pTokenType(t)
      pTokenPred "expect <#{t}>" do |type, text|
        type == t
      end
    end

    def pEmpty
      ParserImpl.new do |toks, stk|
        _output_empty(toks)
      end
    end

    def cSeq(*ps)
      return ps.first if ps.length == 1
      ParserImpl.new do |toks, stk|
        fail_result = nil
        results = ps.map do |p|
          result = p.parse(toks, stk)
          unless result.success?
            fail_result = result
            break
          end
          toks = result.rest
          result
        end
        fail_result || _output_nodes(_merge_results(results), toks)
      end
    end

    def cGlob(*ps)
      parser = cSeq(*ps)
      ParserImpl.new do |toks, stk|
        r = parser.parse(toks, stk)
        r.success? ? _output_empty(r.rest) : r
      end
    end

    def cOr(*ps)
      ParserImpl.new do |toks, stk|
        result = nil
        deepest = nil
        ps.each do |p|
          r = p.parse(toks, stk)
          if r.success?
            result = r
            break
          end
          if deepest.nil? or r.rest.offset > deepest.rest.offset
            deepest = r
          end
        end
        result || deepest || _output_fail('empty OR', toks)
      end
    end

    def cNot(*ps)
      parser = cSeq(*ps)
      ParserImpl.new do |toks, stk|
        tok = toks.car
        next _output_fail('reach eof', toks) if tok.eof?
        r = parser.parse(toks, stk)
        next _output_fail('NOT fail', toks) if r.success?
        _output_node(Node.make_leaf(tok), toks.cdr)
      end
    end

    def cStar(*ps)
      parser = cSeq(*ps)
      ParserImpl.new do |toks, stk|
        results = []
        while true
          r = parser.parse(toks, stk)
          break _output_nodes(_merge_results(results), toks) unless r.success?
          toks = r.rest
          results << r
        end
      end
    end

    def cMaybe(*ps)
      parser = cSeq(*ps)
      cOr(parser, pEmpty)
    end

    def cPlus(*ps)
      parser = cSeq(*ps)
      cSeq(parser, cStar(parser))
    end

    def _env_get(name)
      ex = "#{name.inspect} is undefined"
      value = @env[name]
      raise ex if value.nil?
      value
    end

    def _env_set(name, value)
      @env[name] = value
    end

    def _output_nodes(nodes, rest)
      r = ParseResult.new
      r.nodes = nodes
      r.rest = rest
      r
    end

    def _output_node(node, rest)
      _output_nodes([node], rest)
    end

    def _output_empty(rest)
      _output_nodes([], rest)
    end

    def _output_fail(message, rest)
      r = ParseResult.new
      r.message = message
      r.rest = rest
      r
    end

    def _merge_results(results)
      nodes = []
      results.each do |r|
        r.nodes.each do |node|
          nodes << node
        end
      end
      nodes
    end

    class ParserImpl
      def initialize(&block)
        @parse = block
      end
      def parse(toks, stk)
        @parse.call(toks, stk)
      end
    end

  end


  class ParseResult < Struct.new(:nodes, :rest, :message)
    def success?
      not nodes.nil?
    end
    def pos
      rest.nil? ? -1 : rest.car.start_idx
    end
    def to_tree
      if nodes.nil?
        nil
      else
        nodes.map { |n| n.to_tree }
      end
    end
  end


  class Node < Struct.new(:type, :value, :children, :start_idx, :end_idx)
    def leaf?
      children.nil?
    end
    def to_tree
      if children.nil?
        "<#{value}>"
      else
        nodes = children.map { |n| n.to_tree }
        nodes.unshift(type)
      end
    end
    class << self
      def make_leaf(tok)
        n = Node.new
        n.value=tok.text
        n.start_idx=tok.start_idx
        n.end_idx=tok.end_idx
        n
      end
      def make_node(type, children)
        n = Node.new
        if children.nil? or children.empty?
          n.start_idx = -1
          n.end_idx = -1
        else
          n.start_idx = children.first.start_idx
          n.end_idx = children.last.end_idx
        end
        n.type = type
        n.children = children
        n
      end
    end
  end

  class Grammer
    def initialize(p)
      @scanner = p.scanner
      @parser = p.root_parser
    end
    def parse(str)
      toks = @scanner.scan(str).filter_comment
      r = @parser.parse(toks, [])
      if r.rest.nil? or r.rest.car.eof?
        r
      else
        fr = ParseResult.new
        fr.message = "expect <<EOF>>"
        fr.rest = r.rest
        fr
      end
    end
  end

  def define_grammer(&definition)
    p = Parsec.new
    p.instance_eval(&definition)
    Grammer.new(p)
  end

  module_function :define_grammer

end

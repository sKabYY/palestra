require 'pp'
require './scanner'

module CCParsec

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

    def set_line_comment(line_comment)
      @scanner.line_comment = line_comment
    end

    def set_comment_start(comment_start)
      @scanner.comment_start = comment_start
    end

    def set_comment_end(comment_end)
      @scanner.comment_end = comment_end
    end

    def set_quotation_marks(qms)
      @scanner.quotation_marks = qms
    end

    def set_regex_marks(rms)
      @scanner.regex_marks = rms
    end

    def def_parser(name, parser)
      _env_set(name, parser)
    end

    def get(name)
      ParserImpl.new name do |toks, stk|
        parser = _env_get(name)
        parser.parse(toks, stk)
      end
    end

    def cDebug(*ps)
      parser = cSeq(*ps)
      ParserImpl.new do |toks, stk|
        r = parser.parse(toks, stk)
        puts "===[Debug]===\n#{r.to_sexp.pretty_inspect}"
        r
      end
    end

    def cIs(type, *ps)
      parser = cSeq(*ps)
      ParserImpl.new do |toks, stk|
        r = parser.parse(toks, stk)
        next r unless r.success?
        _output_node(Node.make_node(type, r.nodes, r.start_idx, r.end_idx),
                     r.rest, r.message, r.fail_rest)
      end
    end

    def pTokenPred(fail_message='PRED fail')
      ParserImpl.new do |toks, stk|
        tok = toks.car
        next _output_fail(fail_message, toks) if tok.eof?
        if yield(tok)
          _output_node(Node.make_leaf(tok), toks.cdr)
        else
          _output_fail(fail_message, toks)
        end
      end
    end

    def pEq(s, case_sensitive=true)
      p = pTokenPred "expect \"#{s}\"" do |tok|
        next false unless tok.token?
        if case_sensitive
          tok.text == s
        else
          tok.text.downcase == s.downcase
        end
      end
      cGlob(p)
    end

    def pRegex(regex, fail_message=nil)
      regex = Regexp.new(regex)
      pTokenPred(fail_message || "not match #{regex.inspect}") do |tok|
        next false unless tok.token?
        !!tok.text.match(regex)
      end
    end

    def pTokenType(type)
      pTokenPred "expect <#{type}>" do |tok|
        tok.send "#{type}?"
      end
    end

    def pEmpty
      ParserImpl.new do |toks, stk|
        _output_empty(toks, toks.offset, toks.offset)
      end
    end

    def cSeq(*ps)
      return ps.first if ps.length == 1
      ParserImpl.new do |toks, stk|
        fail_result = nil
        deepest = nil
        results = ps.map do |p|
          r = p.parse(toks, stk)
          unless r.message.nil?
            deepest = r.deeper(deepest)
          end
          unless r.success?
            fail_result = deepest.fail_result
            break
          end
          toks = r.rest
          r
        end
        message = deepest.nil? ? nil : deepest.message
        fail_rest = deepest.nil? ? nil : deepest.fail_rest
        fail_result || _merge_results(results, toks, message, fail_rest)
      end
    end

    def cIfFail(fail_message, parser)
      ParserImpl.new do |toks, stk|
        r = parser.parse(toks, stk)
        r.success? ? r : _output_fail(fail_message, r.rest)
      end
    end

    def cGlob(*ps)
      parser = cSeq(*ps)
      ParserImpl.new do |toks, stk|
        r = parser.parse(toks, stk)
        r.success? ? _output_empty(r.rest, r.start_idx, r.end_idx, r.message, r.fail_rest) : r
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
          deepest = r.deeper(deepest)
        end
        result || deepest || _output_fail('empty OR', toks)
      end
    end

    def cNot(*ps)
      parser = cSeq(*ps)
      ParserImpl.new do |toks, stk|
        tok = toks.car
        next _output_fail('NOT fail: reach eof', toks) if tok.eof?
        r = parser.parse(toks, stk)
        next _output_fail('NOT fail', toks) if r.success?
        _output_node(Node.make_leaf(tok), toks.cdr)
      end
    end

    def cStar(*ps)
      parser = cSeq(*ps)
      ParserImpl.new do |toks, stk|
        results = []
        deepest = nil
        loop do
          r = parser.parse(toks, stk)
          deepest = r.deeper(deepest)
          unless r.success?
            result = _merge_results(results, toks,
                                    deepest.message,
                                    deepest.fail_rest)
            break result
          end
          if toks == r.rest
            Parsec.fatal("empty-star detected", parser, toks, stk)
          end
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
      cSeq(*ps, cStar(*ps))
    end

    def cJoin(sep_parser, *ps)
      cMaybe(cJoinPlus(sep_parser, *ps))
    end

    def cJoinPlus(sep_parser, *ps)
      cSeq(*ps, cStar(sep_parser, *ps))
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

    def _output_nodes(nodes, rest, start_idx, end_idx, message=nil, fail_rest=nil)
      ParseResult.new.tap do |r|
        r.nodes = nodes
        r.rest = rest
        r.message = message
        r.fail_rest = fail_rest || rest
        r.start_idx = start_idx
        r.end_idx = end_idx
      end
    end

    def _output_node(node, rest, message=nil, fail_rest=nil)
      _output_nodes([node], rest, node.start_idx, node.end_idx, message, fail_rest)
    end

    def _output_empty(rest, start_idx, end_idx, message=nil, fail_rest=nil)
      _output_nodes([], rest, start_idx, end_idx, message, fail_rest)
    end

    def _output_fail(message, rest)
      ParseResult.new.tap do |r|
        r.message = message
        r.rest = rest
        r.fail_rest = rest
      end
    end

    def _merge_results(results, rest, message=nil, fail_rest=nil)
      nodes = []
      results.each do |r|
        r.nodes.each do |node|
          nodes << node
        end
      end
      if message.nil? and not results.empty?
        last_r = results.last
        message = last_r.message
        fail_rest = last_r.fail_rest
      end
      start_idx = results.empty? ? rest.offset: results.first.start_idx
      end_idx = results.empty? ? rest.offset: results.last.end_idx
      _output_nodes(nodes, rest, start_idx, end_idx, message, fail_rest)
    end

    class ParserImpl
      def initialize(name=nil, &block)
        @name = name
        @parse = block
      end
      def parse(toks, stk)
        if stk.has?(self, toks)
          Parsec.fatal("left-recursion detected", self, toks, stk)
        end
        @parse.call(toks, stk.extend(self, toks))
      end
      def ==(obj)
        equal?(obj)
      end
      def inspect
        @name.nil? ? super : "<Parser:#{@name}>"
      end
    end

    class << self
      def fatal(message, parser, toks, stk)
        raise message + "\n" +
              "parser: #{parser.inspect}\n" +
              "rest: #{toks.inspect}" +
              "stack trace: #{stk.to_list.pretty_inspect}"
      end
    end

  end


  class ParseResult < Struct.new(:nodes, :rest, :message, :fail_rest, :start_idx, :end_idx)
    def success?
      not nodes.nil?
    end
    def deeper(r)
      if r.nil? or r.fail_rest.offset < fail_rest.offset
        self
      else
        r
      end
    end
    def fail_result
      ParseResult.new(nil, rest, message, fail_rest, start_idx, end_idx)
    end
    def to_sexp
      {success?: success?,
       nodes: nodes && nodes.map { |n| n.to_sexp},
       message: message,
       rest: rest,
       fail_rest: fail_rest}
    end
  end


  class Node < Struct.new(:type, :value, :children, :start_idx, :end_idx)
    def leaf?
      children.nil?
    end
    def to_sexp
      if children.nil?
        "#{value}"
      else
        nodes = children.map { |n| n.to_sexp }
        nodes.unshift("#{type}(#{start_idx}-#{end_idx})".to_sym)
      end
    end
    def leaf_value
      if children.nil? or children.length != 1 or not children.first.leaf?
        raise "can't get leaf_value of #{to_sexp}"
      end
      children.first.value
    end
    class << self
      def make_leaf(tok)
        Node.new.tap do |n|
          n.value=tok.text
          n.start_idx=tok.start_idx
          n.end_idx=tok.end_idx
        end
      end
      def make_node(type, children, start_idx, end_idx)
        Node.new.tap do |n|
          n.start_idx = start_idx
          n.end_idx = end_idx
          n.type = type
          n.children = children
        end
      end
    end
  end

  class Stack < Struct.new(:parser, :toks, :prev)
    def initialize(p=nil, s=nil, pr=nil)
      super(p, s, pr)
    end
    def has?(p, s)
      return true if p == parser and s == toks
      return false if prev.nil?
      prev.has?(p, s)
    end
    def extend(p, s)
      Stack.new(p, s, self)
    end
    def to_list
      stk = self
      list = []
      until stk.nil?
        list << {parser: stk.parser, toks: stk.toks}
        stk = stk.prev
      end
      list
    end
  end

  class Grammer
    def initialize(p)
      @scanner = p.scanner
      @parser = p.root_parser
    end
    def parse(str)
      toks = @scanner.scan(str).filter_comment
      #pp toks.to_list
      r = @parser.parse(toks, Stack.new)
      #pp r.to_sexp
      if r.rest.eof?
        r
      else
        if r.message.nil?
          ParseResult.new.tap do |fr|
            fr.message = "expect <<EOF>>"
            fr.rest = r.rest
            fr.fail_rest = r.rest
          end
        else
          r.fail_result
        end
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

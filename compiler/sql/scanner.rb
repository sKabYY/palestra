module CCParsec

  require 'stringio'

  class Scanner

    attr_accessor :delims
    attr_accessor :line_comment
    attr_accessor :comment_start
    attr_accessor :comment_end
    attr_accessor :operators
    attr_accessor :quotation_marks
    attr_accessor :regex_marks
    attr_accessor :lisp_char
    attr_accessor :significant_whitespaces

    def initialize
      # s-expression settings
      # please override for other languages
      @delims = ['(', ')', '[', ']', '{', '}', '\'', '`', ',']
      @line_comment = [';']
      @comment_start = nil;
      @comment_end = nil;
      @operators = [];
      @quotation_marks = ['"'];
      @regex_marks = [];
      @lisp_char = ['#\\', '?\\']
      @significant_whitespaces = []
    end

    def inspect
      '<Scanner>'
    end

    def scan1(str, start_idx)
      if str.length == start_idx
        return Token.make_eof(nil, start_idx, start_idx)
      end
      text, end_idx = _start_with_one_of(str, start_idx, @significant_whitespaces)
      unless text.nil?
        return Token.make_newline(text, start_idx, end_idx)
      end
      if _whitespace?(str[start_idx])
        return scan1(str, start_idx + 1)
      end
      # line coment
      text, end_idx = _start_with_one_of(str, start_idx, @line_comment)
      unless text.nil?
        end_idx = _find_next(str, end_idx, "\n")
        if end_idx.nil?
          end_idx = str.length
        end
        return Token.make_comment(str[start_idx..end_idx-1], start_idx, end_idx)
      end
      # block comment
      text, end_idx = _start_with(str, start_idx, @comment_start)
      unless text.nil?
        end_idx = _find_next(str, end_idx, @comment_end)
        end_idx = end_idx.nil? ? str.length : end_idx + @comment_end.length
        return Token.make_comment(str[start_idx..end_idx-1], start_idx, end_idx)
      end
      # delim
      text, end_idx = _start_with_one_of(str, start_idx, @delims)
      unless text.nil?
        return Token.make_token(text, start_idx, end_idx)
      end
      # operator
      text, end_idx = _start_with_one_of(str, start_idx, @operators)
      unless text.nil?
        return Token.make_token(text, start_idx, end_idx)
      end
      # string
      mark, end_idx = _start_with_one_of(str, start_idx, @quotation_marks)
      unless mark.nil?
        match_data = /#{mark}(\\.|[^#{mark}])*#{mark}/.match(str, start_idx)
        if match_data.nil?
          raise ScanException.new('string match error', start_idx, nil)
        end
        match_text = match_data[0]
        text = match_text[mark.length..-(mark.length+1)]
        text = text.gsub("\\#{mark}", "#{mark}")
        return Token.make_str(text, start_idx, start_idx + match_text.length)
      end
      # regex
      mark, end_idx = _start_with_one_of(str, start_idx, @regex_marks)
      unless mark.nil?
        match_data = /#{mark}(\\.|[^#{mark}])*#{mark}/.match(str, start_idx)
        if match_data.nil?
          raise ScanException.new('regex match error', start_idx, nil)
        end
        match_text = match_data[0]
        text = match_text[mark.length..-(mark.length+1)]
        text = text.gsub("\\#{mark}", "#{mark}")
        return Token.make_regex(text, start_idx, start_idx + match_text.length)
      end
      # scheme/elisp char
      # TODO
      # identifier or literal type
      buf = StringIO.new
      idx = start_idx
      while idx < str.length
        if not _whitespace?(str[idx]) and
          _start_with_one_of(str, idx, @line_comment).nil? and
          _start_with(str, idx, @comment_start).nil? and
          _start_with(str, idx, @comment_end).nil? and
          _start_with_one_of(str, idx, @quotation_marks).nil? and
          _start_with_one_of(str, idx, @delims).nil? and
          _start_with_one_of(str, idx, @operators).nil?
          buf << str[idx]
          idx += 1
        else
          break
        end
      end
      text = buf.string
      # TODO: literal type
      return Token.make_token(text, start_idx, idx)
    end

    class BaseStream
      def to_list
        lst = []
        s = self
        loop do
          lst << s.car
          break if s.eof?
          s = s.cdr
        end
        lst
      end
    end

    class TokenStream < BaseStream

      attr_reader :str

      def initialize(scanner, str, pos=0)
        @scanner = scanner
        @str = str
        @cur = @scanner.scan1(@str, pos)
        @pos = @cur.end_idx
      end

      def inspect
        "<TokenStream,car=#{car.pretty_str(@str)}>"
      end

      def car
        @cur
      end

      def cdr
        TokenStream.new(@scanner, @str, @pos)
      end

      def eof?
        @cur.eof?
      end

      def offset
        @cur.start_idx
      end

      def filter_comment
        FilteredStream.new self do |tok|
          not tok.comment?
        end
      end

      def ==(obj)
        return false unless obj.is_a?(TokenStream)
        offset == obj.offset and str == obj.str
      end

    end

    class FilteredStream < BaseStream

      def initialize(stream, &block)
        until stream.eof? or yield(stream.car)
          stream = stream.cdr
        end
        @stream = stream
        @pred = block
      end

      def orig_stream
        @stream
      end

      def inspect
        "<FilteredTokenStream,car=#{car.pretty_str(@stream.str)}>"
      end

      def car
        @stream.car
      end

      def cdr
        FilteredStream.new(@stream.cdr, &@pred)
      end

      def offset
        @stream.offset
      end

      def eof?
        self.car.eof?
      end

      def ==(obj)
        return false unless obj.is_a?(FilteredStream)
        orig_stream == obj.orig_stream
      end

    end

    def scan(str)
      return nil if str.length == 0
      TokenStream.new(self, str)
    end

    def _find_next(s, start_idx, text)
      s.index(text, start_idx)
    end

    def _start_with(s, start_idx, prefix)
      return nil if prefix.nil?
      if s[start_idx..-1].start_with?(prefix)
        [prefix, start_idx + prefix.length]
      else
        nil
      end
    end

    def _start_with_one_of(s, start_idx, prefixes)
      prefix = prefixes.detect do |p|
        s[start_idx..-1].start_with?(p)
      end
      prefix.nil? ? nil : [prefix, start_idx + prefix.length]
    end

    def _whitespace?(s)
      s.strip == ''
    end

  end


  class Token < Struct.new(:type, :text, :start_idx, :end_idx)

    class << self

      def token_types
        [:eof, :newline, :comment, :token, :operator, :str, :regex, :literal]
      end

      Token.token_types.each do |type|
        define_method "make_#{type}" do |text, start_idx, end_idx|
          Token.new(type, text, start_idx, end_idx)
        end
      end

    end

    token_types.each do |type|
      define_method "#{type}?" do
        self.type == type
      end
    end

    def _offset_to_position(str, offset)
      linenum = 1
      str.lines.each do |line|
        linelen = line.length
        break if linelen > offset
        offset -= linelen
        linenum += 1
      end
      [linenum, offset]
    end

    def pretty_str(str)
      start_ln, start_cn = _offset_to_position(str, start_idx)
      end_ln, end_cn = _offset_to_position(str, end_idx)
      "#<Token type=:#{type}, text=\"#{text}\", from=(#{start_ln}, #{start_cn}), to=(#{end_ln}, #{end_cn})>"
    end

  end

  class ScanException < StandardError
    attr_reader :start_idx, :end_idx
    def initialize(message, start_idx, end_idx)
      super(message)
      @start_idx = start_idx
      @end_idx = end_idx
    end
  end

end


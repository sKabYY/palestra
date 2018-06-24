require './xccc'
require './match'

module Oracle

  class Analyser
    include CCParsec::Match

    oracle_grammer_code = IO.read('oracle.grammer')
    _grammer = CCParsec::xccc_define_grammer(oracle_grammer_code) do
      @case_sensitive = false
      set_delims ['(', ')', '[', ']', '{', '}', ',', '.', ';']
      set_quotation_marks ['\'']
      set_line_comment ['--']
      set_comment_start '/*'
      set_comment_end '*/'
    end
    define_method :grammer do
      _grammer
    end

    def parse(code)
      grammer.parse(code)
    end

    def analyse(code)
      ast = grammer.parse(code)
      unless ast.success?
        raise "parsing failed: { message: \"#{ast.message}\", rest: #{ast.fail_rest.inspect} }"
      end
      stms = ast.nodes
      stms.map { |stm| analyse_stm(stm) }
        .inject { |s, l| s + l }
    end

    def analyse_stm(stm)
      match stm do
        type :create_table do |table, *coldef_or_constraint|
          puts "creating table #{table.children.map{|n| n.value}} with:"
          pp coldef_or_constraint.map { |c| c.to_tree }
          [1,2]
        end
      end
    end

  end

  def parse(code)
    Analyser.new.parse(code)
  end

  def analyse(code)
    Analyser.new.analyse(code)
  end

  module_function :parse
  module_function :analyse

end

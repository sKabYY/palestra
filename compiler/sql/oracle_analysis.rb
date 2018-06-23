require './xccc'

module Oracle

  oracle_grammer_code = IO.read('oracle.grammer')
  grammer = CCParsec::xccc_define_grammer(oracle_grammer_code) do
    @case_sensitive = false
    set_delims ['(', ')', '[', ']', '{', '}', ',', '.', ';']
    set_quotation_marks ['\'']
    set_line_comment ['--']
    set_comment_start '/*'
    set_comment_end '*/'
  end

  define_method :parse do |code|
    grammer.parse(code)
  end

  define_method :analyse do |code|
  end

  module_function :parse

end

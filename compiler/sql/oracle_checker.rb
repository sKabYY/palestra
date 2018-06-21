require './xccc'

module Oracle

  oracle_grammer_code = IO.read('oracle.grammer')
  grammer = Parsec::xccc_define_grammer(oracle_grammer_code) do
    set_delims ['(', ')', '[', ']', '{', '}', ',', '.', ';']
    set_quotation_marks ['\'']
    set_line_comment ['--']
    set_comment_start '/*'
    set_comment_end '*/'
  end

  define_method :parser do
    grammer
  end

  module_function :parser

end

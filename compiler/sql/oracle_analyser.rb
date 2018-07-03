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
      res = AnalyseResult.new
      stms.each {|stm| interp_stm(stm, res) }
      res
    end

    def interp_stm(stm, res)
      match stm do
        type :create_table do |*ns|
          interp_create_table(*ns, res)
        end
        type :comment do |n_obj, n_content|
          match n_obj do
            type :table_comment do
            end
            type :column_comment do
            end
            type :operator_comment do
            end
            type :indextype_comment do
            end
            type :materialized_view_comment do
            end
          end
        end
        type :create_index do
        end
        type :grant do
        end
        type :create_synonym do |*ns|
          interp_create_synonym(*ns, res)
        end
        # TODO
      end
    end

    def interp_create_table(n_table, n_table_items, res)
      if n_table.children.length == 2
        schema, table_name = n_table.children.map {|n| n.value }
      else
        schema = nil
        table_name = n_table.leaf_value
      end
      t = DbObject.create(Table, table_name, n_table)
      t.schema = schema
      res.env_set(table_name, t)
      n_table_items.children.each do |n_item|
        match n_item do
          type :column_definition do |n_colname, n_datatype, n_c1, n_default, n_c2|
            n_constraints = n_c1.children + n_c2.children
            column = value_of_column_definition(n_colname, n_datatype,
                                                n_default, n_constraints,
                                                res)
            t.add_column(column)
          end
          type :constraint do |n_name, n_constraint|
            constraint = value_of_table_level_constraint(n_name, n_constraint,
                                                         res)
            #t.add_constraint(constraint)
          end
        end
      end
    end

    def value_of_column_definition(n_colname, n_datatype,
                                   n_default, n_constraints,
                                   res)
      colname = n_colname.value
      column = DbObject.create(Column, colname, n_colname)
      column.datatype = value_of_datatype(n_datatype, res)
      column.default = value_of_default(n_default, res)
      n_constraints.each do |n_constraint|
        constraint = value_of_column_level_constraint(n_constraint, res)
        #column.add_constraint(constraint)
      end
      column
    end

    def value_of_datatype(n_datatype, res)
      type_name = n_datatype.type
      n_size = n_datatype.children.first
      DataType.make(type_name, n_datatype).tap do |dt|
        unless n_size.nil?
          dt.size = n_size.value
        end
      end
    end

    def value_of_default(n_default, res)
      return nil if n_default.children.empty?
      rule = n_default.children.first.type
      Default.make(rule, n_default)
    end

    def value_of_column_level_constraint(n_constraint, res)
    end

    def value_of_table_level_constraint(n_name, n_constraint, res)
    end

    def interp_create_synonym(n_replace_option, n_public_option,
                              n_synonym, n_object, res)
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

  class AnalyseResult < Struct.new(:env, :errs)
    def initialize
      self.env = {}
      self.errs = []
    end
    def env_set(name, value)
      if env.has_key?(name)
        # TODO: node?
        errs << ErrorMsg.error("#{name} is defined.", value.node)
      end
      env[name] = value
    end
    def to_sexp
      [[:env, env.map{|k,v| v.to_sexp}],
       [:errs, errs]]
    end
  end

  class ErrorMsg < Struct.new(:level, :message, :node)
    class << self
      def warning(message, node)
        ErrorMsg.new(:warning, message, node)
      end
      def error(message, node)
        ErrorMsg.new(:error, message, node)
      end
    end
  end

  class DbObject
    attr_accessor :name, :node, :status
    def creating?
      status == :creating
    end
    def existed?
      status == :existed
    end
    def to_sexp(type)
      [type, name,
       [:node, "#{node.start_idx}-#{node.end_idx}"],
       [:status, status]]
    end
    class << self
      def _new(cls, name, node, status)
        obj = cls.new
        obj.name = name
        obj.status = status
        obj.node = node
        obj
      end
      def create(cls, name, node)
        _new(cls, name, node, :creating)
      end
      def exist(cls, name, node)
        _new(cls, name, node, :existed)
      end
    end
  end

  class Table < DbObject
    attr_accessor :schema, :columns, :constraints
    def initialize
      @columns = []
      @constraints = []
    end
    def add_column(column)
      # TODO check
      columns << column
    end
    def add_constraint(constraint)
      # TODO check
      constraints << constraint
    end
    def to_sexp
      super(:table).tap do |sexp|
        unless schema.nil?
          sexp << [:schema, schema]
        end
        sexp.push(*columns.map(&:to_sexp), *constraints.map(&:to_sexp))
      end
    end
  end

  class Column < DbObject
    attr_accessor :datatype, :default, :constraints
    def initialize
      @constraints = []
    end
    def add_constraint(constraint)
      # TODO check
      constraints << constraint
    end
    def to_sexp
      super(:column).tap do |sexp|
        sexp << datatype.to_sexp
        unless default.nil?
          sexp << default.to_sexp
        end
        sexp.push(*constraints.map(&:to_sexp))
      end
    end
  end

  class DataType < DbObject
    attr_accessor :size
    def type
      name.to_sym
    end
    def to_sexp
      super(:datatype).tap do |sexp|
        unless size.nil?
          sexp << [:size, size]
        end
      end
    end
    class << self
      def make(type_name, node)
        DbObject.exist(DataType, type_name.to_s, node)
      end
    end
  end

  class Default < DbObject
    attr_accessor :default_rule
    def to_sexp
      super(:default)
    end
    class << self
      def make(rule, node)
        default_rule = rule
        DbObject.create(Default, default_rule, node)
      end
    end
  end

end

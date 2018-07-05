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

    def not_implemented(node)
      raise "not implemented: #{node.to_sexp}"
    end

    def analyse(code)
      ast = grammer.parse(code)
      unless ast.success?
        raise "parsing failed: { message: \"#{ast.message}\", rest: #{ast.fail_rest.inspect} }"
      end
      stms = ast.nodes
      AnalysisResult.new.tap do |res|
        stms.each {|stm| interp_stm(stm, res) }
        AnalysisRules.apply(res)
      end
    end

    def interp_stm(stm, res)
      match stm do
        type :create_table do |*ns|
          interp_create_table(*ns, res)
        end
        type :comment do |n_obj, n_content|
          content = n_content.value
          obj = match n_obj do
            type :table_comment do |n_table|
              schema, tname = value_of_table_name(n_table)
              res.env_get_or(tname, Table.exist(schema, tname, n_table))
            end
            type :column_comment do |n_table, n_column_name|
              schema, tname = value_of_table_name(n_table)
              colname = n_column_name.value
              table = res.env_get_or(tname, Table.exist(schema, tname, n_table))
              table.get_column(colname)
            end
            type :operator_comment do
              not_implemented(n_obj)
            end
            type :indextype_comment do
              not_implemented(n_obj)
            end
            type :materialized_view_comment do
              not_implemented(n_obj)
            end
          end
          obj.set_comment(n_content.value)
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
      schema, table_name = value_of_table_name(n_table)
      t = Table.create(schema, table_name, n_table)
      res.env_set(table_name, t)
      n_table_items.children.each do |n_item|
        match n_item do
          type :column_definition do |n_colname, n_datatype, *n_citems|
            column = value_of_column_definition(n_colname, n_datatype, n_citems, res)
            t.add_column(column, res)
          end
          type :constraint do |n_name, n_constraint|
            constraint = value_of_table_level_constraint(n_name, n_constraint,
                                                         res)
            t.add_constraint(constraint, res)
          end
        end
      end
    end

    def value_of_obj_name_with_schema(n_name)
      if n_name.children.length == 2
        schema, obj_name = n_name.children.map {|n| n.value }
      else
        schema = nil
        obj_name = n_name.leaf_value
      end
      [schema, obj_name]
    end

    def value_of_table_name(n_table)
      value_of_obj_name_with_schema(n_table)
    end

    def value_of_column_definition(n_colname, n_datatype, n_citems, res)
      colname = n_colname.value
      column = DbObject.create(Column, colname, n_colname)
      column.datatype = value_of_datatype(n_datatype, res)
      n_citems.each do |n_item|
        match n_item do
          type :constraint do |*ns|
            constraint = value_of_column_level_constraint(*ns, res)
            column.add_constraint(constraint, res)
          end
          type :default do |n_default|
            column.set_default(value_of_default(n_default, res))
          end
        end
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
      return nil if n_default.nil?
      rule = n_default.type
      Default.make(rule, n_default)
    end

    def value_of_column_level_constraint(n_name, n_constraint, res)
      name = n_name.children.empty? ? nil : n_name.leaf_value
      match n_constraint do
        type :not_null do
          ColumnLevelConstraint.make(name, :not_null, n_constraint)
        end
        type :primary_key do
          ColumnLevelConstraint.make(name, :primary_key, n_constraint)
        end
        type :unique do
          ColumnLevelConstraint.make(name, :unique, n_constraint)
        end
        type :references_clause do
          not_implemented(n_constraint)
        end
      end
    end

    def value_of_table_level_constraint(n_name, n_constraint, res)
      name = n_name.children.empty? ? nil : n_name.leaf_value
      match n_constraint do
        type :primary_key do |*n_colnames|
          colnames = n_colnames.map(&:value)
          TableLevelConstraint.make(name, :primary_key, colnames, n_constraint)
        end
        type :unique do |*n_colnames|
          colnames = n_colnames.map(&:value)
          TableLevelConstraint.make(name, :unique, colnames, n_constraint)
        end
        type :foreign_key do |*n_colnames, n_ref|
          not_implemented(n_constraint)
        end
      end
    end

    def interp_create_synonym(n_replace_option, n_public_option,
                              n_synonym, n_object, res)
      is_replace = not(n_replace_option.children.empty?)
      is_public = not(n_public_option.children.empty?)
      synonym_schema, synonym_name = value_of_obj_name_with_schema(n_synonym)
      obj_schema, obj_name = value_of_obj_name_with_schema(n_object)
      # TODO check
      synonym = Synonym.make(synonym_schema, synonym_name, obj_schema, obj_name,
                             is_replace, is_public, n_synonym)
      obj = res.env_get(obj_name)
      if obj.respond_to?(:add_synonym)
        obj.add_synonym(synonym)
      else
        res.error("cannot create synonym for #{obj_name}", n_object)
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

  class AnalysisResult < Struct.new(:env, :errs)
    def initialize
      self.env = {}
      self.errs = []
    end
    def env_set(name, value)
      if env.has_key?(name)
        error("#{name} is defined.", value.node)
        return
      end
      env[name] = value
    end
    def env_get(name)
      env[name]
    end
    def env_get_or(name, obj)
      if env.has_key?(name)
        env[name]
      else
        env[name] = obj
      end
    end
    def each(objType)
      env.each do |name, obj|
        if obj.is_a?(objType)
          yield(name, obj)
        end
      end
    end
    def warning(message, node)
      errs << ErrorMsg.warning(message, node)
    end
    def error(message, node)
      errs << ErrorMsg.error(message, node)
    end
    def to_sexp
      [[:env, env.map{|k,v| v.to_sexp}],
       [:errs, errs.map(&:to_sexp)]]
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
    def to_sexp
      [level, message, node.to_sexp]
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
    attr_accessor :schema, :columns, :constraints, :comment,
                  :synonyms
    def initialize
      @columns = []
      @constraints = []
      @synonyms = []
    end
    def add_column(column, res)
      if columns.find {|c| c.name.upcase == column.name.upcase }
        res.error("table #{name} already has column #{column.name}",
                  column.node)
        return
      end
      if column.primary_key?
        unless set_primary_key([column.name], column.node, res)
          return
        end
      end
      # TODO check
      columns << column
    end
    def add_constraint(constraint, res)
      if creating?
        colnames = missing_cols(constraint.colnames)
        unless colnames.empty?
          res.error("table #{name} has no columns: #{colnames.join(',')}",
                    constraint.node)
          return
        end
      end
      if constraint.type == :primary_key
        unless set_primary_key(constraint.colnames, constraint.node, res)
          return
        end
      end
      # TODO check
      constraints << constraint
    end
    def add_synonym(synonym)
      synonyms << synonym
    end
    def creating_columns
      columns.select(&:creating?)
    end
    def set_comment(content)
      @comment = content
    end
    def set_primary_key(colnames, node, res)
      if @primary_key.nil?
        @primary_key = [colnames, node]
        true
      else
        res.error("table #{name} already has primary_key(#{@primary_key[0].join(',')})",
                  node)
        false
      end
    end
    def primary_key
      @primary_key
    end
    def missing_cols(names)
      names.select do |name|
        get_column_in_columns(name).nil?
      end
    end
    def get_column_in_columns(colname)
      columns.find{|col| col.name.upcase == colname.upcase }
    end
    def get_column(colname)
      get_column_in_columns(colname)
      # TODO
    end
    def to_sexp
      super(:table).tap do |sexp|
        sexp << [:schema, schema] unless schema.nil?
        sexp << [:comment, comment] unless comment.nil?
        sexp.push(*columns.map(&:to_sexp),
                  *constraints.map(&:to_sexp),
                  *synonyms.map(&:to_sexp))
      end
    end
    class << self
      def create(schema, name, node)
        DbObject.create(Table, name, node).tap do |o|
          o.schema = schema
        end
      end
      def exist(schema, name, node)
        DbObject.exist(Table, name, node).tap do |o|
          o.schema = schema
        end
      end
    end
  end

  class Column < DbObject
    attr_accessor :datatype, :default, :constraints, :comment
    def initialize
      @constraints = []
    end
    def primary_key?
      constraints.find{|c| c.type == :primary_key }
    end
    def add_constraint(constraint, res)
      # TODO check
      constraints << constraint
    end
    def set_default(v)
      # TODO check
      @default = v
    end
    def set_comment(content)
      @comment = content
    end
    def not_null?
      not constraints.find{|c| c.type == :not_null }.nil?
    end
    def to_sexp
      super(:column).tap do |sexp|
        sexp << datatype.to_sexp
        sexp << default.to_sexp unless default.nil?
        sexp << [:comment, comment] unless comment.nil?
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
        sexp << [:size, size] unless size.nil?
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
        DbObject.create(Default, rule, node).tap do |o|
          o.default_rule = rule
        end
      end
    end
  end

  class ColumnLevelConstraint < DbObject
    attr_accessor :type
    def to_sexp
      super(:constraint).tap do |sexp|
        sexp << type
      end
    end
    class << self
      def make(name, type, node)
        DbObject.create(ColumnLevelConstraint, name, node).tap do |o|
          o.type = type
        end
      end
    end
  end

  class TableLevelConstraint < DbObject
    attr_accessor :type, :colnames
    def to_sexp
      super(:constraint).tap do |sexp|
        sexp << [type, colnames]
      end
    end
    class << self
      def make(name, type, colnames, node)
        DbObject.create(TableLevelConstraint, name, node).tap do |o|
          o.type = type
          o.colnames = colnames
        end
      end
    end
  end

  class Synonym < DbObject
    attr_accessor :synonym_schema, :synonym_name,
                  :obj_schema, :obj_name,
                  :is_replace, :is_public
    def replace?
      is_replace
    end
    def public?
      is_public
    end
    def to_sexp
      super(:synonym).tap do |sexp|
        options = []
        options << :replace if replace?
        options << :public if public?
        sexp << options unless options.empty?
        sexp << [:"->", [synonym_schema, synonym_name], [obj_schema, obj_name]]
      end
    end
    class << self
      def make(synonym_schema, synonym_name, obj_schema, obj_name,
               is_replace, is_public, node)
        DbObject.create(Synonym, nil, node).tap do |o|
          o.synonym_schema = synonym_schema
          o.synonym_name = synonym_name
          o.obj_schema = obj_schema
          o.obj_name = obj_name
          o.is_replace = is_replace
          o.is_public = is_public
        end
      end
    end
  end

  class AnalysisRules
    class << self
      rules = []
      define_method :rule do |name, &block|
        rules << [name, block]
      end
      define_method :apply do |res|
        rules.each do |name, block|
          block.call(res)
        end
      end
    end
  end


  class AnalysisRules

    rule :primary_key_name do |res|
      res.each Table do |name, table|
        if table.primary_key.nil?
          if table.creating?
            res.warning("missing primary key", table.node)
          end
        else
          pk_names, node = table.primary_key
          unless pk_names.find{|pk| not pk.upcase.start_with?("PK_") }.nil?
            res.warning("primary key should start with \"PK_\"", node)
          end
        end
      end
    end

    rule :primary_key_constraint do |res|
      res.each Table do |name, table|
        pk_cstr_name = "PK_#{name}".upcase
        pk_constraints = [
          table.constraints.select{|c| c.type == :primary_key },
          table.columns.map{|col| col.constraints.select{|c| c.type == :primary_key } }
        ].flatten
        pk_constraints.each do |c|
          unless c.name.nil? or c.name.upcase == pk_cstr_name
            res.warning("primary key constraint name should be #{pk_cstr_name}",
                        c.node)
          end
        end
      end
    end

    rule :comments do |res|
      res.each Table do |name, table|
        if table.creating? and table.comment.nil?
          res.warning("table #{name} needs a comment", table.node)
        end
        table.creating_columns.each do |column|
          if column.comment.nil?
            res.warning("column #{name}.#{column.name} needs a comment", table.node)
          end
        end
      end
    end

    rule :default_null do |res|
      res.each Table do |name, table|
        table.creating_columns.each do |column|
          if not column.default.nil? and
            column.default.default_rule == :null and
            column.not_null?
            res.warning("column #{name}.#{column.name} is not null",
                        column.default.node)
          end
        end
      end
    end

    # TODO: check whether default value matchs datatype

    # TODO: check synonym for table

  end

end

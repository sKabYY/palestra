require './parsec'

module CCParsec

  class NodeMatcher
    def initialize(ctx, node)
      @ctx = ctx
      @node = node
      @proc_map = {}
      @default = Proc.new do |t|
        raise "match error: unknown type :#{t}"
      end
    end
    def type(t, &proc)
      @proc_map[t] = Proc.new do |*args|
        @ctx.instance_exec(*args, &proc)
      end
    end
    def default(&proc)
      @default = proc
    end
    def do_match()
      proc = @proc_map[@node.type]
      if proc.nil?
        @default.call(@node.type)
      else
        proc.call(*@node.children)
      end
    end
  end

  class Parsec
    def match(node, &block)
      m = NodeMatcher.new(self, node)
      m.instance_eval(&block)
      m.do_match()
    end
  end

end

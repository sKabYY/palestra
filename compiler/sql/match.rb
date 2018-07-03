require './parsec'

module CCParsec

  class NodeMatcher
    def initialize(ctx, node)
      @ctx = ctx
      @node = node
      @proc_map = {}
      @default = Proc.new do |t|
        raise "match error: unknown type :#{t.inspect}(#{t.class})"
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

  module Match
    def match(node, &block)
      unless node.is_a? Node
        raise "#{node.inspect}(#{node.class}) is not a Node"
      end
      m = NodeMatcher.new(self, node)
      m.instance_eval(&block)
      m.do_match()
    end
  end

end

class C
end

c = C.new

c.instance_eval do
  self.class.send :define_method, :f do
    'haha'
  end
end

p c.f

c2 = C.new
p c2.f

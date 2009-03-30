proc{q=1;def q.foo; end}  #q should be varnametoken, both times
module Defined_p_syntax_tests
  def self.defined?(foo) :baz end  #should be methname
  def defined?(foo) :bar end  #should be methname
  def ameth
    p(defined? 44)  #should be keyword
    p(self.defined? 44) #should be methname
  end
end

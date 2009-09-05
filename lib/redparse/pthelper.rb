class RedParse
  def self.remove_silly_begins(pt)
    pt.each_with_index{|x,i|
      if Array===x
        remove_silly_begins(x)
        if x.size==2 and x.first==:begin
          pt[i]=x=x.last
        end
      end
    }
  end
end

__END__
begin require 'rubygems'
rescue LoadError; #do nothing
end

have_graphwalk=true
begin require 'ron/graphedge'
rescue LoadError; 
  warn 'Ron::GraphWalk not found; some tests will be too strict'
  have_graphwalk=false
end

unless have_graphwalk
  class RedParse
    def self.remove_silly_begins(pt) pt end
  end
else
  class RedParse
    def self.remove_silly_begins(pt)
      munger=proc{|cntr,o,i,ty,useit|
        if Array===o and o.size==2 and o.first==:begin
          useit[0]=true
          Ron::GraphWalk.graphcopy(o.last,&munger)
        end
      }
      Ron::GraphWalk.graphcopy(pt,&munger)
    end
  end
end


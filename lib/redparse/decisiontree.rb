=begin           
    redparse - a ruby parser written in ruby
    Copyright (C) 2008, 2012  Caleb Clausen

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
=end

require 'rubygems'
require 'rubylexer'
require 'reg'
require 'tempfile'
require 'pp'




class Class
  class FlattenedHierarchy
    def self.child_relations_among(*classes)
      classes.unshift Object
      result={}
      classes.each{|klass| result[klass]=[] }

      classes.each{|klass| klass.ancestors.each{|anc|
        if anc=result[anc]
          anc << klass
          break
        end
      }}
    
      return result
    end
    alias child_relationships_among child_relations_among

    def self.sort_by_inheiritance(*classes)
      children_of=child_relations_among(*classes)
      result=[]
      class_eater=proc{|klass|
        result << klass
        children_of[klass].each(&class_eater)
      }
      class_eater[::Object]
      #classes.each(&class_eater)

      return result
    end

    def initialize(*classes)
      @sorted=FlattenedHierarchy.sort_by_inheiritance(*classes)
      @ranges=create_member_ranges
    end

    def range klass
      @ranges[klass]
    end
    
    attr :ranges

    def overlapping_class_range_list(classes)
      classes=@sorted&classes
      return classes,classes.map{|k| @ranges[k] }   
    end

    def nonoverlapping_class_range_list(classes)
      classes,ranges=overlapping_class_range_list(classes)

      myclasses=[];myranges=[]
      classes_index_stack=[]
      classes.each_with_index{|k,i| 
        x=ranges[i].first
        if i>0 and ranges[i-1]===x #if overlaps previous range
          classes_index_stack.push i-1 #add to the stack of saved-up ranges
        else
          #pop off old superclasses that no longer apply 
          #to this k, adding trailing fragments for the 
          #ranges of those superclasses as we go along
          until classes_index_stack.empty?
            current_range=ranges[classes_index_stack.last]
            break if current_range===x  #stop if this superclass still applies

            ending=classes_index_stack.pop

            #trailing fragment
            done_thru=myranges.last.last
            current_end=current_range.last
            unless done_thru==current_end
              myranges << (done_thru+1..current_end)
              myclasses << classes[ending]
            end
          end
        end
    
        #if a gap between (sub-?)classes, emit a fragment for the appropriate super (or default to nil)
        next_expected=myranges.last.last+1
        if next_expected!=x  #was:   (ranges[i].huh)
          myclasses<< (classes[classes_index_stack.last] unless classes_index_stack.empty?)
          myranges<<(next_expected..x-1)
        end

        #emit initial fragment for current class
        myclasses << k
        myranges << (x..[ranges[i+1].first-1,ranges[i].last].min)
      }
 
      return myclasses, myranges    
    end

  def nonoverlapping_results_range_list(class2results)
    classes=class2results.keys
    classes,ranges=nonoverlapping_class_range_list(classes)
    return classes.map{|k| class2results[k] }, ranges
  end

  private
    def last_member_num(klass,i)
      @sorted[i]==klass or fail
      low=i
      high=@sorted.size
      until low==high
        mid=(low+high)/2
        if classes[mid]<=klass
          low=mid
        else
          high=mid
        end
      end
 
      return high
    end
 
    def create_member_ranges
      result={}
      @sorted.each_with_index{|klass,i| 
        result[klass]=i..last_member_num(klass,i)
      }
      return result
    end
  end

=begin
  attr_accessor :member_range
  def member_num; member_range.first end

=end

  class DecisionTable<Array
    def initialize(fh,class2result)
      @hierarchy=fh
      classes,ranges=fh.nonoverlapping_results_range_list(class2result)
      newme=[]
      classes.each_with_index{|k,i| newme.push k,ranges[i]}
      replace newme
    end
    class<<self; alias [] new; end

    def inspect
      "Class::DecisionTable"+super
    end
    def pretty_print(q)
      q.group(1, 'Class::DecisionTable[', ']') {
        q.seplist(self) {|v| q.pp v }
      }
    end

    def decide(klass,member_ranges)
      decide_from_classid(member_ranges[klass])
    end
    def decide_from_classid(x)
      huh broken
      if x.send op, val
        use=iftrue
      else
        use=iffalse
      end
      return use.decide_from_classid(x) if DecisionTree===use
      return use
    end

=begin
    def to_ruby
      "
         if (x#{op}#{val}) #{DecisionTree===iftrue ? iftrue.to_ruby : ref_to iftrue};
         else #{DecisionTree===iffalse ? iffalse.to_ruby : ref_to iffalse};
         end
      "
    end

    def to_c
      "
         if (x#{op}#{val}) #{DecisionTree===iftrue ? iftrue.to_c : c_ref_to iftrue};
         else #{DecisionTree===iffalse ? iffalse.to_c : c_ref_to iffalse};
      "
    end
=end

=begin
  def self.overlapping_class_range_list(classes,member_ranges)
    classes=sort_by_inheiritance(*classes)
    return classes,classes.map{|k| member_ranges[k] }   
  end

  def self.nonoverlapping_class_range_list(classes,member_ranges)
    classes,ranges=overlapping_class_range_list(classes,member_ranges)

    myclasses=[];myranges=[]
    classes_index_stack=[]
    classes.each_with_index{|k,i| 
      x=ranges[i].first
      if i>0 and ranges[i-1]===x #if overlaps previous range
        classes_index_stack.push i-1 #add to the stack of saved-up ranges
      else
        #pop off old superclasses that no longer apply to k,
        #adding regions for their last fragment as we go along
        until classes_index_stack.empty?
          current_range=ranges[classes_index_stack.last]
          break if current_range===x  
          ending=classes_index_stack.pop
          done_thru=myranges.last.last
          current_end=current_range.last
          unless done_thru==current_end
            myranges<<(done_thru+1..current_end)
            myclasses<<classes[ending]
          end
        end
      end
    
      #if a gap between (sub-?)classes, emit a fragment for the appropriate super (or default to nil)
      next_expected=myranges.last.last+1
      if next_expected!=x  #was:   (ranges[i].huh)
        myclasses<< (classes[classes_index_stack.last] unless classes_index_stack.empty?)
        myranges<<(next_expected..x-1)
      end

      #emit initial fragment for current class
      myclasses<<k
      myranges<<(x..[ranges[i+1].first-1,ranges[i].last].min)
    }
 
    return myclasses, myranges    
  end

  def self.nonoverlapping_results_range_list(class2results,member_ranges)
    classes=class2results.keys
    classes,ranges=nonoverlapping_class_range_list(classes,member_ranges)
    return classes.map{|k| class2results[k] }, ranges
  end


=end
    def to_ruby(low=0,high=results.size-1)
      #if downto a list of just 1 possibility 
      #then return the corresponding result
      return ref_to self[1+2*low] if high==low
      low<high or fail

      mid=((high+low+0.5)/2).to_i #midpoint of remaining list
      mid_class_id=self[2*mid].first
      "
         if (x<#{mid_class_id}) 
           #{to_ruby(low,mid-1)};
         else 
           #{to_ruby(mid,high)};
         end
      "
    end  

    def to_c(low=0,high=self.size-1)
      #if downto a list of just 1 possibility 
      #then return the corresponding result
      return "return "+c_ref_to(self[1+2*low]) if high==low 
      low<high or fail

      mid=((high+low+0.5)/2).to_i #midpoint of remaining list
      mid_class_id=self[2*mid].first
      "
         if (x<#{mid_class_id}) 
           #{to_c(low,mid-1)};
         else 
           #{to_c(mid,high)};
      "
    end  

    def ref_to obj
      @ruby_refs= defined?(@ruby_refs) ? @ruby_refs+1 : 1
      result="@ruby_ref_#{@ruby_refs}"
      instance_variable_set result, obj
      return result
    end

    def c_ref_to(obj)
      @dont_delete_yet||=[]
      @dont_delete_yet << obj #keep a real ref around so that weak ref in inline c code
                            #keeps pointing to the right object.
      return "(VALUE)0x#{obj.object_id.to_s(16)}L"
    end

    def compile_to_ruby
      eval " def self.decide_from_classid(x)\n#{to_ruby}\n end"
    end

    def compile_to_c
      huh "this prolly won't work cause inline c code has to be in an actual class"
      huh 'so,, need to rewrite as temporary class (module?) for the method to live in... then delegate to that'

      begin
      require 'rubygems'
      rescue Exception
      end
      require 'inline'

      class << self
        inline{|write| write.c %{ 
          static VALUE decide_from_classid(unsigned x){
            #{to_c}
          }
        } }
      end
    end

    def compile
      begin
        compile_to_c
      rescue Exception
        compile_to_ruby
      end
    end
  end

  

=begin
  def Class.create_decision_tree(class2result,member_ranges,sorted_list,default=class2result.default)
    sorted_list=sorted_list&class2result.keys
    _create_decision_tree(class2result,member_ranges,sorted_list,default)
  end

  def Class._create_decision_tree(class2result,member_ranges,sorted_list,default,limits=0..sorted_list.size-1)
    case sorted_list.size
    when 0: default #is this right???
    when 1
      klass=sorted_list.first
      x=member_ranges[klass].last
      if (huh member_ranges[sorted_list[limits.first]].first..member_ranges[sorted_lists[limits.last]].last)===x
        DecisionTree[:<=, x, class2result[klass], default]
      else huh
      end
    else
      mid=sorted_list.size/2
      klass=sorted_list[mid]
      DecisionTree[
        :<, member_ranges[klass].first, 
        _create_decision_tree(class2result,member_ranges,sorted_list[0...mid],default,limits.first..mid-1),
        _create_decision_tree(class2result,member_ranges,sorted_list[mid..-1],default,mid..limits.last)
      ]
    end
  end
=end

end



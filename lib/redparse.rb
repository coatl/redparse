=begin
    redparse - a ruby parser written in ruby
    Copyright (C) 2008,2009, 2012, 2016  Caleb Clausen

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



require 'forwardable'

require 'digest/sha2'

begin
  require 'rubygems'
rescue LoadError=>e
  #hope we don't need it
  raise unless /rubygems/===e.message
end
require 'rubylexer'
require 'reg'
require 'reglookab'

require "redparse/node"
#require "redparse/decisiontree"
require "redparse/reg_more_sugar"
#require "redparse/generate"
require "redparse/cache"
#require "redparse/compile"

class RedParse



  alias :dump :inspect # preserve old inspect functionality
  
  # irb friendly #inspect/#to_s
  def to_s
    mods=class<<self; ancestors; end.reject{|k| !k.name }-self.class.ancestors
    mods=mods.map{|mod| mod.name }.join('+')
    mods="+"<<mods unless mods.empty?
    input=@input||@lexer.input
    "#<#{self.class.name}#{mods}: [#{input.inspect}]>"
  end
  
  alias :inspect :to_s
  
  def pretty_stack max=nil
    target=@stack
    target=target[-max..-1] if max and max<target.size

    target.map{|n| 
      res=n.inspect
      res<<"\n" unless res[-1]=="\n"
      res
    }
  end

####### generic stuff for parsing any(?) language
#  include Nodes
  class StackMonkey
    def initialize(name,first_changed_index,and_expect_node,options={},&monkey_code)
      first_changed_index=-first_changed_index if first_changed_index>0
      @name,@first_changed_index,@and_expect_node,@monkey_code=
        name,first_changed_index,and_expect_node,monkey_code
    end

    attr_reader :name, :first_changed_index, :and_expect_node, :monkey_code
    alias hint and_expect_node
    attr_accessor :exemplars

    def [](stack)
      result=@monkey_code[stack]
      return result
    end

    def _dump depth
      @name
    end

    def self._load str
      Thread.current[:$RedParse_parser].undumpables[@name]
    end

    def action2c
      #"return the whole thing on first call, just a goto stmt after that"
      return "    goto #@goto_label;\n" if defined? @goto_label

=begin
      <<-E
      #{@goto_label=@name.gsub(/[^a-z0-9_]/,'_')}:
        monkey=rb_hash_get(undumpables,rb_cstr2str("#@name"));
        rb_funcall(monkey,rb_intern("[]"),huh_stack);

        /*recover from stackmonkey fiddling*/
        for(i=0;i<#{-@first_changed_index};++i) {
          rb_ary_unshift(lexer_moretokens,
            rb_ary_pop(huh_semantic_stack));
          rb_ary_pop(huh_syntax_stack);
        }

        goto #{Node===@and_expect_node ? 
                 postreduceaction4this_state(@and_expect_node) : 
                 shiftaction4this_state
        };
      E
=end
    end
  end
  class DeleteMonkey<StackMonkey
    def initialize(index,name)
      index=-index if index>0
      @index=index
      super(name,index,nil){|stack| stack.delete_at( index )}
    end
  end
  def stack_monkey(*args,&block) StackMonkey.new(*args,&block) end
  def self.stack_monkey(*args,&block) StackMonkey.new(*args,&block) end
  def delete_monkey(index,name) DeleteMonkey.new(index,name) end

  def evaluate rule
    #dissect the rule
if false
    rule=rule.dup
    lookahead_processor=(rule.pop if Proc===rule.last)
    node_type=rule.pop
else
    Reg::Transform===rule or fail
    node_type= rule.right
    rule=rule.left.subregs.dup
    lookahead_processor=(rule.pop if Proc|::Reg::LookAhead===rule.last)
    lookback=rule[0]=rule[0].subregs[0] if ::Reg::LookBack===rule[0]
end
    
    #index of data at which to start matching
    i=@stack.size-1   #-1 because last element of @stack is always lookahead

=begin was, but now done by expanded_RULES
    #I could call this a JIT compiler, but that's a bit grandiose....
    #more of a JIT pre-processor
    compiled_rule=@compiled_rules[rule]||=
      rule.map{|pattern| 
        String|Regexp===pattern ? KW(pattern) : pattern 
      }
=end
    assert(rule.grep(String|Regexp|Reg::Subseq|Reg::LookAhead|Reg::LookBack|Proc).empty?)
    compiled_rule=rule

    #what's the minimum @stack size this rule could match?
    rule_min_size=@min_sizes[compiled_rule]||=
      compiled_rule.inject(0){|sum,pattern| 
        sum + pattern.itemrange.begin 
      }
    i>=rule_min_size or return false

    matching=[]

    #actually try to match rule elements against each @stack element in turn
    compiled_rule.reverse_each{|matcher|
      i.zero? and fail
      target=matching
      #is this matcher optional? looping?
      loop= matcher.itemrange.last.to_f.infinite?
      minimum=matcher.itemrange.first
      optional=minimum.zero?
      matching.unshift target=[]  if loop
      if loop or optional
        matcher=matcher.subregs[0]
      end

      begin
        if matcher===@stack[i-=1]  #try match
          target.unshift @stack[i]
        else
          #if match failed, the whole rule fails
          #unless this match was optional, in which case, ignore it
          #or was looping and met its minimum
          #but bump the data position back up, since the latest datum
          #didn't actually match anything.
          return false unless optional or loop&&target.size>=minimum
          i+=1
          matching.unshift nil unless loop
          break
        end
      end while loop
    } 

    matchrange= i...-1  #what elems in @stack were matched?

    #give lookahead matcher (if any) a chance to fail the match
    case lookahead_processor
    when ::Reg::LookAhead
      return false unless lookahead_processor.subregs[0]===@stack.last
    when Proc
      return false unless lookahead_processor[self,@stack.last] 
    end

    #if there was a lookback item, don't include it in the new node
    if lookback
      matchrange= i+1...-1  #what elems in @stack were matched?
      matching.shift
    end


    #replace matching elements in @stack with node type found
    case node_type
    when Class
        node=node_type.create(*matching)
        node.startline||=@stack[matchrange.first].startline
        node.endline=@endline
        @stack[matchrange]=[node]
    when Proc,StackMonkey;   node_type[@stack]
    when :shift; return 0
    when :accept,:error; throw :ParserDone
    else fail
    end
    
    return true #let caller know we found a match

    
  rescue Exception #=>e
    #puts "error (#{e}) while executing rule: #{rule.inspect}"
    #puts e.backtrace.join("\n")
    raise
  end


  def coalesce_loop(klass=nil,ident=nil,klass2=nil,ident2=nil)
    eligible=rules.reverse.map!{|rule| can_coalesce?(rule,klass,ident,klass2,ident2)&&rule }
    i=rules.size
    eligible.map!{|rule| 
      i-=1
      next unless rule
      if @size_cache
        @size_cache[[i,rule.right]]||=1
        @size_cache[[i,rule.right]]+=1
      end
      coalesce rule, i, klass,ident,klass2,ident2
    }
    eligible.compact!
    @size_cache[klass2 ? [klass,ident,klass2,ident2] : ident ? ident : klass]= eligible.size if @size_cache

    @empty_reduce_withs+=1 if defined? @empty_reduce_withs and eligible.size.zero?

    return eligible
  end

  def can_coalesce? rule,klass=nil,ident=nil,klass2=nil,ident2=nil
    Reg::Transform===rule or fail
    node_type= rule.right
    rule=rule.left.subregs.dup
    rule.pop if Proc|::Reg::LookAhead===rule.last
    rule[0]=rule[0].subregs[0] if ::Reg::LookBack===rule[0]

=begin was, but now done by expanded_RULES
    #I could call this a JIT compiler, but that's a bit grandiose....
    #more of a JIT pre-processor
    compiled_rule=@compiled_rules[rule]||=
      rule.map{|pattern| 
        String|Regexp===pattern ? KW(pattern) : pattern 
      }
=end
    assert(rule.grep(String|Regexp|Reg::Subseq|Reg::LookAhead|Reg::LookBack|Proc).empty?)


    return false if klass && !can_combine?(rule,klass,ident)
    return false if klass2 && !can_combine2?(rule,klass2,ident2,-2)
    warn "plain lit matches #{node_type}" if klass==LiteralNode and klass2.nil?
    return true
  end

  def coalesce rule,rulenum,klass=nil,ident=nil,klass2=nil,ident2=nil
    #last 4 params aren't actually neeeded anymore

    @coalesce_result||=[]
    result=@coalesce_result[rulenum]
    return result if result

    #dissect the rule
    Reg::Transform===rule or fail
    node_type= rule.right
    rule=rule.left.subregs.dup
    lookahead_processor=(rule.pop if Proc|::Reg::LookAhead===rule.last)
    lookback=rule[0]=rule[0].subregs[0] if ::Reg::LookBack===rule[0]

    assert @rules[rulenum].right==node_type

    if klass==VarNode and klass2==KeywordToken
      #warn "can_combine2? about to fail"
    end

    needends=0
    result=["\n##{mui node_type}\n"]
    
    #index of data at which to start matching
    result<<"i=@stack.size-1   ##{mui node_type}\n#-1 because last element of @stack is always lookahead\n"

=begin was, but now done by expanded_RULES
    #I could call this a JIT compiler, but that's a bit grandiose....
    #more of a JIT pre-processor
    compiled_rule=@compiled_rules[rule]||=
      rule.map{|pattern| 
        String|Regexp===pattern ? KW(pattern) : pattern 
      }
=end
    assert(rule.grep(String|Regexp|Reg::Subseq|Reg::LookAhead|Reg::LookBack|Proc).empty?)
    compiled_rule=rule

    return if klass && !can_combine?(compiled_rule,klass,ident) #should never happen
    return if klass2 && !can_combine2?(compiled_rule,klass2,ident2,-2) #should never happen

    #what's the minimum @stack size this rule could match?
    rule_min_size=@min_sizes[compiled_rule]||=
      compiled_rule.inject(0){|sum,pattern| 
        sum + pattern.itemrange.begin 
      }
    if rule_min_size > 1
      needends+=1
      result<<"if i>=#{rule_min_size}\n"
      min_i=rule_min_size
    end
    #@@has_loop||=[]
    #@@has_optional||=[]
    has_loop=#@@has_loop[rulenum]||=
      compiled_rule.find{|x| x.itemrange.last.to_f.infinite? }
    has_optional=#@@has_optional[rulenum]||=
      compiled_rule.find{|x| x.itemrange.first.zero? }

    if Class===node_type and has_loop||has_optional
      result<<"matching=[]\n"
      need_matching=true
    end

    j=compiled_rule.size
    #actually try to match rule elements against each @stack element in turn
    first1=true
    compiled_rule.reverse_each{|matcher|
      j-=1
      result<<"i.zero? and fail\n" unless min_i && min_i>0 or first1
      first1=false
      #is this matcher optional? looping?
      maximum= matcher.itemrange.last
      minimum= matcher.itemrange.first
      loop= maximum.to_f.infinite?
      optional=minimum.zero?
      fail "looping matcher with finite maximum not supported" if maximum>1 and !loop
      if need_matching
        success="matching.unshift item"
        loopsuccess="target.unshift item"
        optfail="matching.unshift nil"

        result<<"matching.unshift target=[]\n" if loop
      end
      is_lookback=matcher .equal? lookback
      if loop or optional
        matcher=matcher.subregs[0]
        fail "lookback is not a scalar" if is_lookback
      end

      itemget="@stack[i-=1]"
      itemget="(item=#{itemget})" if success
      test="#{ref_to matcher,rulenum,j}===#{itemget}  #try match of #{mui matcher}"
      p [:misparse_start, matcher] if node_type===MisparsedNode and j.zero?
      matcher= ~ (matcher.subregs[0]|NilClass) if Reg::Not===matcher
      if matcher===nil and j.zero?
        warn "rule ##{rulenum}(>>#{node_type}) can match nil at start; might match emptiness before start of stack"
      end
      if !loop
        fail unless maximum==1
        min_i-=1 if min_i
        result<<<<-END
            if #{test}
              #{success if !is_lookback}
                   END
        optional ? result<<<<-END : needends+=1
            else
              #ignore optional match fail
              #but bump the data position back up, since the latest datum
              #didn't actually match anything.
              i+=1
              #{optfail}
            end
                   END
      else
        min_i=nil
        if minimum<10
          needends+=minimum
          result<<<<-END*minimum
          if #{test}
            #{loopsuccess}
          END
          result<<<<-END
            while #{test}
              #{loopsuccess}
            end
              #but bump the data position back up, since the latest datum
              #didn't actually match anything.
              i+=1
          END
        else
          needends+=1
          result<<<<-END
            #{"n=#{minimum}" unless need_matching}
            while #{test}
              #{loopsuccess || "n-=1"}
            end
            if #{need_matching ? "target.size>=minimum" : "n<=0"} then
              #but bump the data position back up, since the latest datum
              #didn't actually match anything.
              i+=1
          END
        end

      end
    } 

    #give lookahead matcher (if any) a chance to fail the match
    result<<case lookahead_processor
    when ::Reg::LookAhead
      action_idx=compiled_rule.size+1
      needends+=1
      "if #{ref_to lookahead_processor.subregs[0],rulenum,compiled_rule.size}===@stack.last ##{mui lookahead_processor.subregs[0] }\n"
    when Proc
      action_idx=compiled_rule.size+1
      needends+=1
      "if #{ref_to lookahead_processor,rulenum,compiled_rule.size}[self,@stack.last] ##{mui lookahead_processor}\n"
    else ''
    end

    #if there was a lookback item, don't include it in the matched set
    #result<<"matching.shift\n"    if lookback and need_matching

    need_return=true

    #replace matching elements in @stack with node type found
    result<<
    case node_type
    when Class
      #if there was a lookback item, don't include it in the new node
      <<-END
        #{"i+=1" if lookback}
        matchrange= i...-1  #what elems in @stack were matched?
        #{"matching=@stack.slice! matchrange" unless need_matching}
        node=#{ref_to node_type,rulenum,action_idx||rule.size}.create(*matching) ##{mui node_type}
        node.startline||=#{need_matching ? "@stack[i]" : "matching.first"}.startline
        node.endline=@endline
        #{need_matching ? "@stack[matchrange]=[node]" : "@stack.insert i,node" }
      END
    when Proc,StackMonkey;   ref_to(node_type,rulenum,action_idx||rule.size)+"[@stack] ##{mui node_type}\n"
    when :shift; need_return=false; "return 0\n"
    when :accept,:error; need_return=false; "throw :ParserDone\n"
    else fail
    end
    
    result<<"return true #let caller know we found a match\n" if need_return
    result<<"end;"*needends
    result<<"\n"

    return @coalesce_result[rulenum]=result
  rescue Exception  #=>e
    #puts "error (#{e}) while executing rule: #{rule.inspect}"
    #puts e.backtrace.join("\n")
    raise
  end

  @@ref_to_cache={}
  @@ref_to_cache_by_id={}
  @@ref_to_idx=-1
  def ref_to obj,i,j
    assert j<=0x3FF
    if Module===obj and obj.name
      return obj.name
    elsif ref=@@ref_to_cache_by_id[obj.__id__] || @@ref_to_cache[(i<<10)+j] 
      return ref
    else
      @@ref_to_rules||=
          rules.map{|rule|
            rule.left.subregs.map{|pat|
              case pat
              when String,Regexp #not needed anymore...?
                RedParse::KW(pat)
              when Reg::LookBack,Reg::LookAhead,Reg::Repeat #Reg::Repeat should be handled already by now
                pat.subregs[0]
              #subseqs handled already
              else pat
              end
            }<<rule.right
          }

      @ref_to_code||=[]
      name="@@ref_#{@@ref_to_idx+=1}"
      #eval "#{name}=obj"
      unless @@ref_to_rules[i][j]==obj
        warn "ref_to mismatch"
      end
      @ref_to_code<<"#{name}=rules[#{i}][#{j}]"
      @ref_to_code<<"warn_unless_equal #@@ref_to_idx,mui(#{name}),#{squote mui( obj )}"
      @@ref_to_cache[(i<<10)+j]=name
      @@ref_to_cache_by_id[obj.__id__]=name
    end
  end

  module ReduceWithUtils
    #a version of inspect that is especially likely to be stable;
    #no embedded addresses and ivar order is always the same
    def matcher_unique_inspect(m)
      result=m.inspect
      return result unless /\A#<[A-Z]/===result
      "#<#{m.class}: "+
         m.instance_variables.sort.map{|iv| 
           val=m.instance_variable_get(iv).inspect
           val.gsub!(/#<(Proc|(?:Stack|Delete)Monkey):(?:0[xX])?[0-9a-fA-F]+/){ "#<#$1:" }
           iv.to_s+"="+val
         }.join(" ")+">"
    end
    alias mui matcher_unique_inspect

    def squote(str)
      "'#{str.gsub(/['\\]/){|ch| %[\\]+ch }}'"
    end

    @@unequal_parser_ref_vars=0
    @@line_mismatch_parser_ref_vars=0
    def warn_unless_equal i,ref,orig
      return if ref==orig
      msg="expected @ref_#{i} to == #{squote orig}, saw #{squote ref}"
      ref=ref.gsub(/\.rb:\d+>/,".rb:X>")
      orig=orig.gsub(/\.rb:\d+>/,".rb:X>")
      count=
        if ref==orig
          msg="@ref_#{i} differed in line nums"
          warn "more @ref_ vars differed in line nums..." if @@line_mismatch_parser_ref_vars==1
          @@line_mismatch_parser_ref_vars+=1
        else
          @@unequal_parser_ref_vars+=1
        end
      warn msg if 1==count
    end
  end
  include ReduceWithUtils

  def classes_matched_by(matcher)
    result=[]
    worklist=[matcher]
    begin
      case x=worklist.shift
      when Reg::And,Reg::Or; worklist.concat x.subregs
      when Class; result<<x
      end
    end until worklist.empty?
    return [Object] if result.empty?
    return result
  end
 


  def can_combine? rule,klass,ident
    rule.reverse_each{|matcher|
      if Reg::Repeat===matcher
        optional= matcher.times.first==0
        matcher=matcher.subregs[0]
      end
      if ident
        return true if matcher===klass.new(ident)
        optional ? next : break
      end

=begin was
      orlist= Reg::Or===matcher ? matcher.subregs : [matcher]
      orlist.map!{|m| 
        classes=(Reg::And===m ? m.subregs : [m]).grep(Class)
        case classes.size
        when 0; return true
        when 1
        else warn "multiple classes in matcher #{matcher}"
        end
        classes if classes.all?{|k| klass<=k }
      }
      return true if orlist.compact.flatten[0]
=end
      return true if classes_matched_by(matcher).any?{|k| klass<=k }

      break unless optional
    }
    return false
  end

  def can_combine2? rule,klass,ident,index=-1
  #very similar to can_combine?, just above
  #i think can_combine2? with 3 params is equiv to can_combine?
  #so, the two should be merged
    index=-index
    rule_max_size=rule.inject(0){|sum,pattern|
        sum + pattern.itemrange.end
    }
    return true if rule_max_size<index
    min=max=0
    rule.reverse_each{|matcher|
      break if index<min
      if Reg::Repeat===matcher
        #optional= matcher.times.first==0
        min+=matcher.times.first
        max+=matcher.times.last
        matcher=matcher.subregs[0]
      else
        min+=1
        max+=1
      end
      next if index>max
      if ident
        return true if matcher===klass.new(ident)
        next #was: optional ? next : break
      end
=begin was
      orlist= Reg::Or===matcher ? matcher.subregs : [matcher]
      orlist.map!{|m| 
        classes=(Reg::And===m ? m.subregs : [m]).grep(Class)
        case classes.size
        when 0; return true
        when 1
        else warn "multiple classes in matcher #{matcher}: #{classes.inspect}"
        end
        classes if classes.all?{|k| klass<=k }
      }
      return true if orlist.compact.flatten[0]
=end
      return true if classes_matched_by(matcher).any?{|k| klass<=k }
    }
    return false
  end

  class ParseError<RuntimeError
    def initialize(msg,stack)
      super(msg)
      @stack=stack
if false
      ranges=(1..stack.size-2).map{|i|
        node=stack[i]
        if node.respond_to? :linerange
          node.linerange
        elsif node.respond_to? :endline
          node.endline..node.endline
        end
      }
      types=(1..stack.size-2).map{|i| stack[i].class }
      msg += "couldn't interpret #{types.inspect} at line ranges: #{ranges.inspect}"
end
      super(msg)
    end
    attr :stack
  end

  def [](*args)
    @stack.[](*args)
  end

  def []=(*args)
    @stack.[]=(*args)
  end

  #try all possible reductions
  def old_slow_reduce
      shift=nil
      @rules.reverse_each{|rule|
        shift=evaluate(rule) and break
      }
      return shift
  end

  HASHED_REDUCER=!ENV['REDUCE_INTERPRETER']

  @@rules_compile_cache={}

  #try all possible reductions
  def reduce
    code=@@rules_compile_cache[class<<self; ancestors end.reject{|k| !k.name}<<@rubyversion]||=coalesce_loop().join
    code= <<-END
      class RedParse
      def (Thread.current['$RedParse_instance']).reduce
      #{code}
      return nil
      end
      end
    END

    f=Tempfile.new("reduce")
    Thread.current['$RedParse_instance']=self
    p [:code_hash, code.hash]
    f.write code
    f.flush
    load f.path

    reduce
  ensure f.close if f
  end if !HASHED_REDUCER


#  include StackableClasses

  Punc2name={
    "("=>"lparen",    ")"=>"rparen",
    "["=>"lbracket",    "]"=>"rbracket",
    "{"=>"lbrace",    "}"=>"rbrace",
    ","=>"comma",
    ";"=>"semicolon",
    "::"=>"double_colon",
    "."=>"dot",
    "?"=>"question_mark", ":"=>"colon",
    "="=>"equals",
    "|"=>"pipe",
    "<<"=>"leftleft", ">>"=>"rightright",
    "=>"=>"arrow",
    "->"=>"stabby",
    "rhs,"=>"rhs_comma",
    "lhs,"=>"lhs_comma",
    "||="=>"or_equals",
    "&&="=>"and_equals",
  }


  RUBYUNOPERATORS=::RubyLexer::RUBYUNOPERATORS
  RUBYBINOPERATORS=::RubyLexer::RUBYBINOPERATORS
  RUBYSYMOPERATORS=::RubyLexer::RUBYSYMOPERATORS
  RUBYNONSYMOPERATORS=::RubyLexer::RUBYNONSYMOPERATORS
  OPERATORS=RUBYUNOPERATORS-%w[~@ !@]+RUBYBINOPERATORS+RUBYNONSYMOPERATORS+
              %w[while until if unless rescue and or not unary* unary& rescue3 lhs* rhs*]
  OPERATORS.uniq!
  RUBYKEYWORDLIST=( 
   RubyLexer::RUBYKEYWORDLIST+Punc2name.keys+
   RUBYSYMOPERATORS+RUBYNONSYMOPERATORS
  ).uniq

  def rubyoperatorlist; OPERATORS end
  def rubykeywordlist; RUBYKEYWORDLIST end

  class KeywordToken
    def reducer_method(stack)
      :"reduce_with_tos_KeywordToken_#@ident"
    end
    def reducer_ident
      :"KeywordToken_#@ident"
    end
  end

  class OperatorToken
    def reducer_ident
      :"OperatorToken_#@ident"
    end
  end

  class ValueNode
    def reducer_method(stack)
      :"reduce_with_tos_#{stack[-3].reducer_ident}_then_#{reducer_ident}"
    end
  end

  def parser_identity
  #what is the relationship between this method and #signature?
  #can the two be combined?
    result=class<<self; ancestors end.reject!{|k| !k.name}
    result.reject!{|k| !!((::RedParse<k)..false) }
    result.reject!{|k| k.name[/^(?:RedParse::)?ReduceWiths/] }
    result.reverse!
    result.push @rubyversion
    #@rubyversion in identity is a hack; should have RedParse1_9 module instead
  end

  def code_for_reduce_with ident, code
     code=coalesce_loop(*code) if Array===code
     ident.gsub!(/[\\']/){|x| "\\"+x}
     code=code.join
     @reduce_with_defns+=1
     if name=@reduce_with_cache[code]
       @reduce_with_aliases+=1
       "alias :'reduce_with_tos_#{ident}' :'#{name}'\n"
     else
       @reduce_with_cache[code]=name="reduce_with_tos_#{ident}"
       ["define_method('", name ,"') do\n", code ,"\nnil\nend\n"]
     end
  end

  def addl_node_containers; [] end

  def write_reduce_withs path=nil
    return unless HASHED_REDUCER
    start=Time.now
    @size_cache={}
    identity=parser_identity
    @reduce_with_cache={}
    @reduce_with_aliases=0
    @empty_reduce_withs=@reduce_with_defns=0

      expanded_RULES()
      shortnames=[]   #[[],[]]
      list=[self.class,*addl_node_containers].map{|mod| 
        mod.constants.select{|k| 
          /(?:Node|Token)$/===k.to_s 
        }.map{|k| 
          mod.const_get k
        }
      }.flatten.grep(Class).uniq
      #list=STACKABLE_CLASSES()
      list -= [KeywordToken,ImplicitParamListStartToken,ImplicitParamListEndToken,
               Token,WToken,NewlineToken,DecoratorToken,Node,ValueNode]
      list.reject!{|x| IgnoreToken>=x and not /(^|:)AssignmentRhs/===x.name}
      exprclasses,list=list.partition{|k| k<=ValueNode }
      fail unless list.include? StartToken
      indexcode=list.map{|klass|
        shortname=klass.to_s[/[^:]+$/]
        warn "empty reducer_ident for ::#{klass}" if shortname.empty?
        <<-END
          class ::#{klass}
            def reducer_method(stack)
              :reduce_with_tos_#{shortname}
            end if instance_methods(false).&(["reducer_method",:reducer_method]).empty?
            def reducer_ident
              :#{shortname}
            end if instance_methods(false).&(["reducer_ident",:reducer_ident]).empty?
          end
        END
      }.concat(exprclasses.map{|exprclass|
        shec=exprclass.name[/[^:]+$/]
        warn "empty reducer_ident for ::#{exprclass}" if shec.empty?
        <<-END
            class ::#{exprclass}
              def reducer_ident
                :#{shec}
              end if instance_methods(false).&(["reducer_ident",:reducer_ident]).empty?
            end
        END
      })
      ruby=["#Copyright (C) #{Time.now.year} #{ENV['COPYRIGHT_OWNER']||'Caleb Clausen'}\n"+
            "#Generated with ruby v#{RUBY_VERSION}\n"
      ].concat list.map{|klass|
        shortname=klass.to_s[/[^:]+$/]
        shortnames<<[shortname,klass,nil]
        code_for_reduce_with( shortname, [klass] )
      }.concat(rubykeywordlist.map{|kw|
        shortname="KeywordToken_#{kw}"
        shortnames<<[shortname,KeywordToken,kw]
        code_for_reduce_with( shortname, [KeywordToken, kw] )
      }).concat({ImplicitParamListStartToken=>'(',ImplicitParamListEndToken=>')'}.map{|(k,v)|
        shortnames<<[k.name,k,v]
        code_for_reduce_with k.name, [k,v]
      })
      shortnames.delete ["OperatorToken",OperatorToken,nil]
      record=shortnames.dup
      ruby.concat(exprclasses.map{|exprclass|
        shec=exprclass.name[/[^:]+$/]
        shortnames.map{|(sn,snclass,snparam)|
          warn "empty shortname for #{snclass}" if sn.empty?
          record<<["#{sn}_then_#{shec}", exprclass, nil, snclass, snparam]
          code_for_reduce_with "#{sn}_then_#{shec}", [exprclass, nil, snclass, snparam]
        } 
      })
      ruby.concat(exprclasses.map{|exprclass|
        shec=exprclass.name[/[^:]+$/]
        rubyoperatorlist.map{|op|
          record<<["OperatorToken_#{op}_then_#{shec}", exprclass, nil, OperatorToken, op]
          code_for_reduce_with "OperatorToken_#{op}_then_#{shec}", [exprclass, nil, OperatorToken, op]
        } 
      }).concat([LiteralNode,VarNode].map{|k|
          shec=k.name[/[^:]+$/]
          record<<["#{shec}_then_#{shec}", k, nil, k, nil]
          code_for_reduce_with "#{shec}_then_#{shec}", [k, nil, k, nil]
      })

      modname="ReduceWithsFor_#{parser_identity.join('_').tr(':.','_')}"

      size_cache,rule_popularity=@size_cache.partition{|((i,action),size)| Integer===i }

      ruby.unshift [<<-END,@ref_to_code.join("\n"),<<-END2]
        #number of coalescences: #{size_cache.size}
        #empty coalescences: #@empty_reduce_withs
        #duplicate coalescences: #@reduce_with_aliases
        #nonduplicate coalescences: #{@reduce_with_cache.size}
        #reduce_with_defns: #@reduce_with_defns
        extend RedParse::ReduceWithUtils
        def self.redparse_modules_init(parser)
          return if defined? @@ref_0
          rules=parser.rules.map{|rule|
            rule.left.subregs.map{|pat|
              case pat
              when String,Regexp #not needed anymore...?
                RedParse::KW(pat)
              when Reg::LookBack,Reg::LookAhead,Reg::Repeat #Reg::Repeat should be handled already by now
                pat.subregs[0]
              #subseqs handled already
              else pat
              end
            }<<rule.right
          }
                                          END

        end
        def redparse_modules_init
          ::RedParse::#{modname}.redparse_modules_init(self) 
          super
        end
                                          END2

      ruby.unshift( "#15 largest coalescences:\n", 
        *size_cache.sort_by{|(k,size)| size}[-15..-1].map{ \
          |(k,size)| "##{k.inspect}=#{size}\n" 
      })
 
      ruby.unshift("#10 most popular rules:\n",
        *rule_popularity.sort_by{|(rule,pop)| pop}[-10..-1].map{ \
          |((i,action),pop)| "##{i} #{action.inspect}=#{pop}\n" 
      })

      warn "15 largest coalescences:"
      size_cache.sort_by{|(klass,size)| size}[-15..-1].each{ \
        |(klass,size)| warn "#{klass.inspect}=#{size}" 
      }

      warn "10 most popular rules:"
      rule_popularity.sort_by{|(rule,pop)| pop}[-10..-1].each{ \
        |((i,action),pop)| warn "#{i} #{action.inspect}=#{pop}" 
      }


      @ref_to_code=nil
      ruby=["module RedParse::#{modname}\n",ruby,"\nend\n",indexcode]
      @@rules_compile_cache[identity]=ruby

    path ||= $LOAD_PATH.find{|d| File.exist? File.join(d,"redparse.rb") }+"/redparse/"
    #should use reduce_withs_directory here somehow instead...

    path += modname+".rb" if path[-1]==?/
    File.open(path,"wb") {|f| ruby.flatten.each{|frag| f.write frag } }

    #warn "actual write_reduce_withs writing took #{Time.now-start}s"
    warn "size of #{path}: #{File.size path}"

  ensure 
    warn "write_reduce_withs took #{Time.now-start}s" if start
    @reduce_with_cache=nil if @reduce_with_cache
    @size_cache=nil if @size_cache
  end

  def old_reduce_loop
    catch(:ParserDone){ loop {
      #try all possible reductions
      next if reduce==true 
      
      #no rule can match current @stack, get another token 
      tok=get_token  or break

      #are we done yet?
      #tok.nil? or EoiToken===tok && EoiToken===@stack.last and break

      #shift our token onto the @stack
      @stack.push tok
    }}
  end

=begin should be
  reduce_call= HASHED_REDUCER ? 
      'send(@stack[-2].reducer_method(@stack))' : 
      'reduce'
  eval <<-END,__FILE__,__LINE__
    def reduce_loop
      catch(:ParserDone){ ( @stack.push(get_token||break) unless(#{reduce_call}==true) ) while true }
    end
  END
=end
  def reduce_loop
    catch(:ParserDone){ while true
      #try all possible reductions
      #was: next if reduce==true
      next if send(@stack[-2].reducer_method(@stack))==true

      #no rule can match current @stack, get another token
      tok=get_token  or break

      #are we done yet?
      #tok.nil? or EoiToken===tok && EoiToken===@stack.last and break

      #shift our token onto the @stack
      @stack.push tok
    end }
  end

  if ENV['REDUCE_INTERPRETER']
    alias reduce old_slow_reduce
    alias reduce_loop old_reduce_loop
  end

  def parse

    #hack, so StringToken can know what parser its called from
    #so it can use it to parse inclusions
    oldparser=Thread.current[:$RedParse_parser]
    Thread.current[:$RedParse_parser]||=self

    return @cached_result if defined? @cached_result

    expanded_RULES()
#    @inputs||=enumerate_exemplars

    @stack=[StartToken.new, get_token] 
           #last token on @stack is always implicitly the lookahead
    reduce_loop

    @stack.size==2 and return result=NopNode.new #handle empty parse string

    #unless the @stack is 3 tokens, 
    #with the last an Eoi, and first a StartToken
    #there was a parse error
    unless @stack.size==3
      puts( pretty_stack( 15 ))if ENV['PRINT_STACK']
      top=MisparsedNode.new("(toplevel)", @stack[1...-1],'')
      raise ParseError.new(top.msg,@stack)
    end
    EoiToken===@stack.last or fail
    StartToken===@stack.first or fail

    result= @stack[1]


    #multiple assignment must be resolved 
    #afterwards by walking the parse tree.
    #(because the relative precedences of = and , 
    #are reversed in multiple assignment.)
#    result.respond_to? :fixup_multiple_assignments! and
#      result=result.fixup_multiple_assignments!

    #relative precedence of = and rescue are also inverted sometimes
#    result.respond_to? :fixup_rescue_assignments! and 
#      result=result.fixup_rescue_assignments!

    #do something with error nodes
    msgs=[]
    result.walk{|parent,i,subi,node|
      if node.respond_to? :error? and node.error?(@rubyversion)
        msgs<< @filename+":"+node.blame.msg
        false
      else
        true
      end
    } if result.respond_to? :walk #hack hack
    result.errors=msgs unless msgs.empty?
    #other types of errors (lexer errors, exceptions in lexer or parser actions)
    #should be handled in the same way, but currently are not
#    puts msgs.join("\n")

=begin
  rescue Exception=>e
    if ENV['PRINT_PARSE_ERRORS']
      input=@lexer
      if Array===input
        STDERR.puts "error while parsing:"
        STDERR.write input.pretty_inspect
        input=nil
      else
        input=input.original_file
#        inputname=@lexer.filename
        STDERR.puts "error while parsing #@filename:#@endline: <<<  #{input.inspect if input.inspect.size<=1000}  >>>"
      end
      e.backtrace.each{|l| p l }
    end
    raise
  else
=end
    unless msgs.empty?
      pp @stack[-[15,@stack.size].min..-1] if ENV['PRINT_STACK']
      raise RedParse::ParseError.new(msgs.join("\n"),@stack)
    end

#    result=NopNode.new if EoiToken===result
    return result
  ensure
    @write_cache.put(@input,result) if @write_cache and result and !result.errors
    @stack=nil
    Thread.current[:$RedParse_parser]=oldparser
  end


  #HIER=Class::FlattenedHierarchy.new *STACKABLE_CLASSES

  def new_disabled_reduce
    #@hier||=Class::FlattenedHierarchy.new *STACKABLE_CLASSES()
    @reducer||=Reducer.new(@rules)

    @reducer.reduce(@stack)
  end #


  #inline any subsequences in RULES right into the patterns
  #reg should do this already, but current release does not
  #also expand regexp/string to keyword matcher
  def expanded_RULES
    return @rules if defined? @rules
    result=RULES()
    #return result if (-[:foo, -[:bar]]).subregs.grep(Reg::Subseq).empty?
    @rules=result.map!{|rule|
      if rule.left.subregs.grep(Reg::Subseq|String|Regexp).empty?
      then rule
      else
        right=rule.right
        rule=rule.left.subregs.dup
        (rule.size-1).downto(0){|i|
          case mtr=rule[i]
          when Reg::Subseq
            rule[i,1]=mtr.subregs
          when String,Regexp
            rule[i]=RedParse::KW(mtr)
          end
        }
        -rule>>right
      end
    }
  end

  ###### specific to parsing ruby


  UCLETTER=RubyLexer::UCLETTER

  LCLETTER=RubyLexer::LCLETTER
  LETTER=RubyLexer::LETTER
  LETTER_DIGIT=RubyLexer::LETTER_DIGIT

  def vertices; self.class.constants.grep(Node|Token) end

  def self.has_return_hash_fix? #is this needed? it's not used in this file....
    rl=RubyLexer.new("","return {}.size")
    return(
      FileAndLineToken===rl.get1token and
      MethNameToken===rl.get1token and
      ImplicitParamListStartToken===rl.get1token and
      WsToken===rl.get1token and
      KeywordToken===rl.get1token and
      KeywordToken===rl.get1token and
      KeywordToken===rl.get1token and
      MethNameToken===rl.get1token and
      ImplicitParamListStartToken===rl.get1token and
      ImplicitParamListEndToken===rl.get1token and
      ImplicitParamListEndToken===rl.get1token and
      EoiToken===rl.get1token
    )
  end

  #see pickaxe, 1st ed, page 221
  def RIGHT_ASSOCIATIVE
    {
#    "defined?"=>120.5,
    "**"=>118,   

    "="=>105,    "%="=>105,   "/="=>105,   "-="=>105,    "+="=>105,
    "|="=>105,   "&="=>105,   ">>="=>105,  "<<="=>105,   "*="=>105,
    "&&="=>105,  "||="=>105,  "**="=>105,  "^="=>105,


#    "and"=>99, "or"=>99,

#   "if"=>98, "unless"=>98, "while"=>98, "until"=>98, "rescue"=>98, 

#    "&&"=>109, "||"=>108,
    }
  end
  
  def PRECEDENCE
    {

  #  "("=>122,     #method param list
  #  "{"=>122,    "do"=>122,    #blocks

    "::"=>121,    "."=>121,

 #   "defined?"=>120.5, 

    "["=>120,     #[] []= methods

    "!"=>119,    "~"=>119,
    "+@"=>119, 

    "**"=>118,   

    "-@"=>117, 

    "*"=>116,    "/"=>116,    "%"=>116,

    "+"=>115,    "-"=>115,

    "<<"=>114,   ">>"=>114,
 
    "&"=>113,

    "^"=>112,    "|"=>112,

    "<="=>111,   ">="=>111,   "<"=>111,    ">"=>111,

    "<=>"=>110,  "=="=>110,   "==="=>110,  
    "!="=>110,   "=~"=>110,   "!~"=>110,

    "&&"=>109,

    "||"=>108,

    ".."=>107, "..."=>107,

    "?"=>106, # ":"=>106,    #not sure what to do with ":"
  
    "unary&"=>105, #unary * and & operators
      "lhs*"=>105,  #this should remain above =
    "lhs,"=>105, 
    "rescue3"=>105,

    "="=>104,    "%="=>104,   "/="=>104,   "-="=>104,    "+="=>104,
    "|="=>104,   "&="=>104,   ">>="=>104,  "<<="=>104,   "*="=>104,
    "&&="=>104,  "||="=>104,  "**="=>104,  "^="=>104,

    "defined?"=>103,
    "not"=>103,
    ":"=>102, #but not when used as a substitute for 'then'

    "=>"=>101,
           "rhs,"=>100, #"call,"=>100, "array,"=>100, "param,"=>100,
    ","=>100, "rhs*"=>100, "unary*"=>100, 
      #the 'precedence' of comma is somewhat controversial. it actually has
      #several different precedences depending on which kind of comma it is.
      #the precedence of , is higher than :, => and the assignment operators 
      #in certain (lhs) contexts. therefore, the precedence of lhs-comma should 
      #really be above "=".

    #"unary" prefix function names seen has operators have this precedence
    #but, rubylexer handles precedence of these and outputs fake parens 
    #to tell us how its parsed

    "or"=>99,   "and"=>99,

    "if"=>98,    "unless"=>98,    "while"=>98,    "until"=>98,

    "rescue"=>98,

    ";"=>96,
    }
  end

  module BracketsCall; end
  Value=     #NumberToken|SymbolToken|
        #HerePlaceholderToken|
        ValueNode&-{:lvalue =>nil}
  Expr=Value

if defined? SPECIALIZED_KEYWORDS
  class SpecializedKeywordToken<KeywordToken
    def inspect
      "#<"+self.class.name+">"
    end
    alias image inspect
  end

  KW2class={}
end

  def self.KW(ident)
if defined? SPECIALIZED_KEYWORDS
    fail if /\\/===ident
    orig_ident=ident
    if Regexp===ident
      list=ident.to_s[/\(?-mix:\^\((.*)\)\$\)/,1]
      
      #pick apart any char class in ident
      if open_bracket_idx=list.index(/([^\\]|^)\[/)
        open_bracket_idx+=1 unless list[open_bracket_idx]=="["
        close_bracket_idx=list.index(/[^\\]\]/,open_bracket_idx+1)
        close_bracket_idx+=1 unless list[close_bracket_idx]=="]"
        cclass=list.slice!(open_bracket_idx..close_bracket_idx)
        cclass=cclass[1...-1]
        cclass=cclass.scan( /[^\\]|\\./ )
        cclass.map!{|ch| ch.size==1 ? ch : ch[1..1] }
      end

      #rest of it should be a list of words separated by |
      list=list.split(/\|/).reject{|x| x==''}
      list.concat cclass if cclass
      list.map{|w| 
        w.gsub!(/\\/,'')
        KW(w) 
      }.inject{|sum,kw| sum|kw}
    else
      fail unless String===ident
      ident=Punc2name[ident] unless /^(?:(?!#{LETTER_DIGIT}).)+$/o===ident
      fail "no name for #{orig_ident}" unless ident
      eval %{
        class Keyword_#{ident} < SpecializedKeywordToken
          def ident; '#{orig_ident}' end
#         def self.instance; @instance ||= allocate end
#         def self.new; instance end
          def initialize(offset)
            @offset=offset
          end
        end
      }
      KW2class[ident]||=const_get("Keyword_#{ident}")
    end
else
    ident=case ident
          when Integer;        ident.chr
          when String,Regexp;  ident
          else                 ident.to_s
          end

    return KeywordToken&-{:ident=>ident}
end
  end
  def KW(ident); self.class.KW(ident) end

if defined? SPECIALIZED_KEYWORDS
  def make_specialized_kw(name,offset)
    name=Punc2name[name] unless /^((?!#{LETTER_DIGIT}).)+$/o===name
    KW2class[name].new(offset)
  end
  alias make_kw make_specialized_kw
else
  def make_kw(name,offset)
    KeywordToken.new(name,offset)
  end
end

  UNOP=
  (OperatorToken|KeywordToken)&-{  #sppflt! KeywordToken here is a hack too
    :ident=>/^(?:[+-]@|unary[&*]|(?:lhs|rhs)[*])$/,
#    :ident=>/^(?:[+-]@|unary[&])$/,
    #:unary =>true,
  }|
  (OperatorToken|KeywordToken)&-{  #sppflt! KeywordToken here is a hack too
    :ident=>/^([~!]|not|defined\?)$/, #defined? should be removed from here, its handled separately
  } #|
  DEFOP=
  (OperatorToken|KeywordToken)&-{  #sppflt! KeywordToken here is a hack too
    :ident=>"defined?",
  } 
=begin
  MethNameToken&-{ #hack, shouldn't be necessary
     #rubylexer should know to generally treat "defined?" as a keyword
     #or operator. (like most keywords, it can also be used as a method 
     #               name....)
    :ident=>"defined?"
  }
=end

  def self.Op(ident=nil, allow_keyword=false)  
    result=OperatorToken
    result |= KeywordToken if allow_keyword
    result &= -{:ident=>ident} if ident
    #result[:infix?]=true
    return result
  end
  def Op(*args); self.class.Op(*args); end
  BINOP_KEYWORDS=%w[if unless while until and or && \|\|]
 
  #HAS_PRECEDENCE=Op(/^#{PRECEDENCE.keys.map{|k| Regexp.quote k}.join('|')}$/,true)
=begin
  KeywordOp=
    KeywordToken & -{
      :ident=>/^(#{BINOP_KEYWORDS.join('|')})$/
    }
  KeywordOp2= 
    KeywordToken & -{ 
      :ident=>/^([\[({!+*?:,]|\.{1,3}|::|=>)$/ 
    }
=end
  DotOp= KW('.') #KeywordToken & -{ :ident=>"." }
  DoubleColonOp= KW('::') #KeywordToken & -{ :ident=>"::" }
 
  Op=Op()
  MODIFYASSIGNOP=Op( /^(([^=])\2|[^<>=!])=$/, true )
  NONASSIGNOP=Op( /([^=]|[<>=!]=)$/)
  KW_Op= #some of these ought to be regular operators, fer gosh sake
    Op(/^(![=~]|\.\.\.?|=>)$/,true)|Op(/^(#{BINOP_KEYWORDS.join('|')})$/)

  EPSILON=Float::EPSILON*10_000_000    #this should be <<1 and >0 
  fail unless  1+EPSILON>1
  fail unless EPSILON<0.1

  def left_op_higher(op,op2)
    KeywordToken===op2 or OperatorToken===op2 or return true
    rightprec=@precedence[op2.to_s] or return true
    rightprec+=EPSILON if @RIGHT_ASSOCIATIVE[op2.to_s]
    return @precedence[op.to_s]>=rightprec
  end

#  LowerOp=     proc{|parser,op2| parser.left_op_higher(parser[-3],op2) }
  module LowerOp_inspect
    def inspect; "lower_op" end
  end

  def lower_op
    @lower_op||=proc{|parser,op| 
      LOWEST_OP===op or (!(parser.VALUELIKE_LA() === op) and 
        parser.left_op_higher(parser.stack[-3],op)
      ) 
    }.extend LowerOp_inspect
=begin was
    return @lower_op if defined? @lower_op
    lower_op=item_that{|op| left_op_higher(@stack[-3],op) }
    lower_op=(LOWEST_OP|(~VALUELIKE_LA() & lower_op)).la
    lower_op.extend LowerOp_inspect
    @lower_op=lower_op
=end
  end

  #this is a hack, should use graphcopy to search for Deferreds and replace with double-Deferred as below
  def item_that(*a,&b)
    if defined? @generating_parse_tables
      fail unless b
      #double supers, one of them in a block executed after this method returns....
      #man that's weird
      super(*a){|ob| @saw_item_that[[super(*a,&b),ob]]=true}
    else
      super(*a,&b) #and then here's another
    end
  end

  WANTS_SEMI=%w[while until if unless 
          def case when in rescue 
          elsif class module << => . ::
          ]
  def wants_semi_context
    Op(/^(<<|=>|\.|::)$/)|KW(/^(#{WANTS_SEMI.map{|ws| Regexp.quote ws }.join('|')})$/)
  end
  def dont_postpone_semi
    @dps||=~wants_semi_context
  end

  #NeverBlockKeyword=MethNameToken&-{:ident=>/^(return|break|next)$/}
  #FakeBegin=KW('(')&-{:not_real? =>true}
  #FakeEnd=KW(')')&-{:not_real? =>true}

  #rule format: 
  # -[syntax pattern_matchers.+, lookahead.-]>>node type

  DotCall=stack_monkey("DotCall",4,CallNode){|stack|
    left,dot=*stack.slice!(-4..-3)
    right=stack[-2]
 
    right.startline=left.startline
    right.set_receiver! left
  }

  Lvalue=(VarNode|CallSiteNode|BracketsGetNode|CommaOpNode|
          ParenedNode|ConstantNode|UnaryStarNode)&-{:lvalue =>true}

  BareMethod=MethNameToken|(LiteralNode&-{:bare_method=>true})

  #BEGINWORDLIST=RubyLexer::BEGINWORDLIST + %w"( [ {"  
  ENDWORDLIST=%w"end ) ] }"
  ENDWORDS=ENDWORDLIST.map{|x| Regexp.quote x}.join('|')
  BEGINWORDS=RubyLexer::BEGINWORDS
  INNERBOUNDINGWORDS=RubyLexer::INNERBOUNDINGWORDS

  BEGIN2END={"{"=>"}", "("=>")", "["=>"]", BEGINWORDS=>"end"}
  def beginsendsmatcher
    @bem||=
    /^(#{BEGINWORDS}|#{ENDWORDS})$/
  end

  MULTIASSIGN=UnaryStarNode|CommaOpNode|ParenedNode
  WITHCOMMAS=UnaryStarNode|CommaOpNode|(CallSiteNode&-{:with_commas=>true})
      #(CallSiteNode&-{:real_parens=>false, :args=>-{:size=>~0.reg}})

  BEGINAFTEREQUALS=
    BeginNode&
      -{:after_equals =>nil}&-{:non_empty=>true}
  BEGINAFTEREQUALS_MARKED=
    BeginNode&
      -{:after_equals =>true}&-{:non_empty=>true}
  
  LHS_COMMA=Op('lhs,',true)#&-{:tag => :lhs}
  RHS_COMMA=Op('rhs,',true)#&-{:tag => :rhs}
  #PARAM_COMMA=Op('param,',true)#&-{:tag => :param}
  def FUNCLIKE_KEYWORD
    KeywordToken&-{:ident=>@funclikes} 
  end
  IGN_SEMI_BEFORE=KW(/^(#{RubyLexer::INNERBOUNDINGWORDS.gsub(/(rescue|then)\|/,'')[1...-1]}|end|[)}\]])$/)|EoiToken
  IGN_SEMI_AFTER=KW(/^(begin|[;:({|]|then|do|else|ensure)$/)|BlockFormalsNode

  #for use in lookback patterns
  OPERATORLIKE_LB=OperatorToken|
                 KW(/^(not | defined\? | rescue3 | .*[@,] | [~!;\(\[\{?:] | \.{1,3} | :: | => | ![=~])$/x)|
                 KW(%r{^( \*\*? | << | >> | &&? | \|\|? | \^ | % | / | - | \+ )?=$}x)|
                 KW(BEGINWORDS)|KW(/^#{INNERBOUNDINGWORDS}$/)|RescueHeaderNode|StartToken|
                 GoalPostToken|BlockFormalsNode|AssignmentRhsListStartToken

  #for use in lookahead patterns
  def VALUELIKE_LA
    @valuelike_la ||=

    KW(@varlikes)|NumberToken|SymbolToken|StringToken|UNOP|DEFOP|
    KW(/^[({]$/x)|VarNameToken|MethNameToken|HerePlaceholderToken|
    KW(BEGINWORDS)|FUNCLIKE_KEYWORD()|AssignmentRhsListStartToken

    #why isn't this a sufficient implementation of this method:
         # KW('(')
    #in which case, '(' can be made the highest precedence operator instead
  end
  LOWEST_OP=KW(/^(#{ENDWORDS})$/)|KW(/^#{INNERBOUNDINGWORDS.sub('rescue|','')}$/)|
            EoiToken|GoalPostToken|AssignmentRhsListEndToken

  RESCUE_BODY=-[Expr.-, RescueNode.*, ElseNode.-, EnsureNode.-,]

  RESCUE_OP=Op('rescue') #|(KW('rescue')&-{:infix=>true})

  RESCUE_KW=KW('rescue')&-{:infix=>nil}

  inspect_constant_names if respond_to? :inspect_constant_names

  def RULES
    lower_op= lower_op()


    result=
    [-[StartToken.lb, Expr.-, EoiToken.la]>>:accept,
     -[EoiToken]>>:error,
    ]+

    #these must be the lowest possible priority, and hence first in the rules list
    BEGIN2END.map{|_beg,_end| 
      -[KW(_beg), (KW(_beg)|KW(_end)).~.*, KW(_end), KW(/^(do|\{)$/).~.la]>>MisparsedNode
    }+

    [
    -[UNOP, Expr, lower_op]>>UnOpNode,
    -[DEFOP, ParenedNode]>>UnOpNode,
    -[Op(/^(?:unary|lhs|rhs)\*$/), ValueNode, lower_op]>>UnaryStarNode,

#    -[Op('=',true)|KW(/^(rescue|when|\[)$/)|Op(/,$/,true),
#      Op(/^(?:unary|rhs)\*$/), ValueNode, (MODIFYASSIGNOP|Op('=',true)).la]>>:shift,
#    -[MethNameToken|FUNCLIKE_KEYWORD(), KW('('), 
#      Op(/^(?:unary|rhs)\*$/), ValueNode, (MODIFYASSIGNOP|Op('=',true)).la]>>:shift,
    #star should not be used in an lhs if an rhs or param list context is available to eat it.
    #(including param lists for keywords such as return,break,next,rescue,yield,when)

    -[Op(/^(?:unary|lhs)\*$/), (GoalPostToken|Op(/,$/,true)|KW(/^(in|[=)|;])$/)).la]>>DanglingStarNode, #dangling *
    -[Op(/,$/,true), (GoalPostToken|KW(/^(in|[=)|;])$/)).la]>> #dangling ,
      stack_monkey("DanglingComma",1,DanglingCommaNode){|stack| 
        dcomma=DanglingCommaNode.new
        dcomma.offset=stack.last.offset
        stack.push dcomma, stack.pop
      },
    #hmmm.... | in char classes above looks useless (predates GoalPostToken)

#    -[Expr, Op|KW_Op, Expr, lower_op]>>RawOpNode,  #most operators
    -[Expr, Op, Expr, lower_op]>>RawOpNode,  #most operators
    -[Expr, "=>", Expr, lower_op]>>ArrowOpNode,

    #assignment
    -[Lvalue, MODIFYASSIGNOP, Expr, lower_op]>>AssignNode,
    -[Lvalue, Op('=',true), AssignmentRhsNode, lower_op]>>AssignNode,
    -[AssignmentRhsListStartToken, Expr, AssignmentRhsListEndToken]>>AssignmentRhsNode,

    # a = b rescue c acts like a ternary,,,
    #provided that both a and b are not multiple and b
    #(if it is a parenless callsite) has just 1 param
#    -[Lvalue&~MULTIASSIGN, Op('=',true), AssignmentRhsNode&-{:is_list=>true}, 
#           Op('rescue3'), Expr, lower_op]>>AssignNode,
    -[Lvalue, Op('=',true), AssignmentRhsNode, Op('rescue3'), Expr, lower_op]>>AssignNode,

#    -[Lvalue&~MULTIASSIGN, Op('=',true), AssignmentRhsNode&-{:is_list=>true}, 
#        Op('rescue3',true).la]>>:shift,

#    -[Lvalue&~MULTIASSIGN, Op('=',true), AssignmentRhsNode&-{:is_list=>true}, 
#        RESCUE_OP.la] >>
#        stack_monkey("rescue3",1,Op('rescue3',true)){|stack| 
#          resc=stack.last.dup
#          resc.ident += '3'
#          stack[-1]=resc
#        },
    #relative precedence of = and rescue are to be inverted if rescue
    #is to the right and assignment is not multiple.

    #if assignment rhs contains commas, don't reduce til they've been read
    #(unless we're already on an rhs)
    -[(Op('=',true)|Expr).~.lb, Lvalue, Op('=',true), Expr, RHS_COMMA.la]>>:shift,
    -[RHS_COMMA.lb, Lvalue, Op('=',true), Expr, RHS_COMMA.la ]>>AssignNode,
    -[ValueNode, LHS_COMMA, ValueNode, Op('=',true).la]>>CommaOpNode,
    #relative precedence of = and lhs/rhs , are to be inverted.
  
    #mark parentheses and unary stars that come after lhs commas
    -[LHS_COMMA, (UnaryStarNode|ParenedNode)&~-{:after_comma =>true}, Op('=',true)]>>
      stack_monkey("after_comma",3,(UnaryStarNode|ParenedNode)&-{:after_comma =>true}){|stack| 
        stack[-3].after_comma=true}, 
               #mebbe this should be a lexer hack?

    -[#(OPERATORLIKE_LB&~Op('=',true)).lb, 
      Expr, RESCUE_OP, Expr, lower_op]>>RescueOpNode,

    #dot and double-colon
    -[DoubleColonOp, VarNode,  lower_op]>>ConstantNode,#unary ::
    -[Expr, DotOp, CallNode, lower_op]>>DotCall,      #binary .
    -[Expr, DoubleColonOp, CallNode, lower_op]>>DotCall,    #binary ::
    -[Expr, DoubleColonOp, VarNode, lower_op]>>ConstantNode,#binary ::
    #lower_op constaints on lookahead are unnecessary in above 4 (unless I give openparen a precedence)

    -[Expr, "?", Expr, ":", Expr, lower_op]>>TernaryNode,


    -[MethNameToken, '(', Expr.-, ')', BlockNode.-, KW('do').~.la]>>CallNode,
    -[FUNCLIKE_KEYWORD(), '(', Expr.-, ')', BlockNode.-, KW('do').~.la]>>KWCallNode,

    -[#(OPERATORLIKE_LB&
      (MethNameToken|FUNCLIKE_KEYWORD()).~.lb, 
      '(', Expr, KW(')')&~(-{:callsite? =>true}|-{:not_real? =>true}), KW('do').~.la]>>ParenedNode,

    -[#(OPERATORLIKE_LB&
      (MethNameToken|FUNCLIKE_KEYWORD()).~.lb, 
      '(', KW(')')&~(-{:callsite? =>true}|-{:not_real? =>true}), KW('do').~.la]>>VarLikeNode, #(), alias for nil
    #constraint on do in above 2 rules is probably overkill

    -[ValueNode, Op(/,$/,true), ValueNode, lower_op]>>CommaOpNode,

    -[(OPERATORLIKE_LB&dont_postpone_semi).lb, 
      Expr, ';', Expr, lower_op]>>SequenceNode,


    -[#(OPERATORLIKE_LB&~KW(')')).lb, 
      '{', (CommaOpNode|ArrowOpNode).-, '}']>>HashLiteralNode, #-40

    -[KW(')').lb, 'do', BlockFormalsNode.-, Expr.-, 'end']>>BlockNode,
    #this does {} as well... converted to do...end
    #rubylexer handles the 'low precedence' of do...end

    -[GoalPostToken, Expr.-, GoalPostToken]>>BlockFormalsNode,
    #rubylexer disambiguated operator vs keyword '|'

    -[/^(while|until)$/, Expr, /^([:;]|do)$/, Expr.-, 'end']>>LoopNode,

    -[/^(if|unless)$/, Expr, /^(;|then|:)$/, 
      Expr.-, ElsifNode.*, ElseNode.-, 'end'
     ]>>IfNode,

    -['else', Expr.-, KW(/^(ensure|end)$/).la]>>ElseNode,

    -['elsif', Expr, /^(;|then|:)$/, Expr.-,
      KW(/^(end|else|elsif)$/).la
     ]>>ElsifNode,

#     -['module', ConstantNode|VarNode, KW(/^(;|::)$/).~.la]>>
#       stack_monkey(1,KW(';')){|stack| #insert ; at end of module header if none was present
#         stack.push KeywordToken.new(';'), stack.pop
#       },
    -['module', ConstantNode|VarNode, ';', RESCUE_BODY, 'end']>>ModuleNode,
    -['class', Expr, ';', RESCUE_BODY, 'end']>>ClassNode,
    -['class', Expr, Op('<'), Expr, KW(';').~.la]>>:shift,
    -['class', Op('<<'), Expr, ';', RESCUE_BODY, 'end']>>MetaClassNode,  #-30

    -['alias', BareMethod|VarNode, BareMethod|VarNode]>>AliasNode,
    -['undef', BareMethod]>>UndefNode,
    -[UndefNode, Op(',',true), BareMethod]>>UndefNode,

    -['def', CallSiteNode, Op('=').-, KW(';'), RESCUE_BODY,
#        Expr.-, RescueNode.*, ElseNode.-, EnsureNode.-, 
      'end'
    ]>>MethodNode,

    -['begin', RESCUE_BODY,
 #       Expr.-, RescueNode.*, ElseNode.-, EnsureNode.-, 
      'end'
    ]>>BeginNode,

    -[Op('=',true), BEGINAFTEREQUALS, RESCUE_OP.la]>>
      stack_monkey("begin after equals",2,BEGINAFTEREQUALS_MARKED){ |stack| stack[-2].after_equals=true }, 
    #this is bs. all for an extra :begin in the parsetree

    -[(KW(/^(;|begin)$/)|RescueNode).lb, #ParenedNode|RescueOpNode|BeginNode used to be here too
      RESCUE_KW, KW('=>').-, Expr.-, /^([:;]|then)$/,
    ]>>RescueHeaderNode,
    -[ RescueHeaderNode, Expr.-, KW(';').-, (KW(/^(else|ensure|end)$/)|RESCUE_KW).la
    ]>>RescueNode,

    -['ensure', Expr.-, KW('end').la]>>EnsureNode,

    -['[', Expr.-, ']']>>ArrayLiteralNode, #-20

    -[Expr, '[', Expr.-, ']']>>BracketsGetNode,

    -[HereDocNode, StringToken+1, StringToken.~.la]>>StringCatNode,  
    -[(OPERATORLIKE_LB&~(StringToken|HereDocNode)).lb, StringToken+2, StringToken.~.la]>>StringCatNode,  
    -[(OPERATORLIKE_LB&~(StringToken|HereDocNode)).lb, StringToken, StringToken.~.la]>>StringNode,  
      #includes regexp, wordlist, backquotes

    -['case', Expr.-, KW(';').-, WhenNode.*, ElseNode.-, 'end']>>CaseNode,

    -['when', Expr, /^([:;]|then)$/, Expr.-, 
     KW(/^(when|else|end)$/).la
    ]>>WhenNode,            

    -['for', Expr, 'in', Expr, /^([:;]|do)$/, Expr.-, 'end']>>ForNode,

    #semicolon cleanup....
    -[(OPERATORLIKE_LB&dont_postpone_semi).lb,Expr, ';', IGN_SEMI_BEFORE.la] \
                                                     >>delete_monkey(2,"semi_cleanup_before_ISB"),
    -[Expr, ';', KW('then').la]                      >>delete_monkey(2,"semi_cleanup_before_then"),
    -[dont_postpone_semi.lb, Expr, ';', RescueNode]  >>delete_monkey(3,"semi_cleanup_before_rescue"),   #-10
    -[IGN_SEMI_AFTER.lb, ';']                        >>delete_monkey(2,"semi_cleanup_after_oplike"),
    -[(StartToken|RescueHeaderNode).lb, ';' ]        >>delete_monkey(2,"semi_cleanup_after_rescue"),
     #this rule is somewhat more forgiving than matz' parser...
     #not all semicolons after :, (, and { keywords should 
     #be ignored. some should cause syntax errors.

 
    #comma cleanup....
    -[Op(/,$/,true), KW(/^([}\]])$/).la]             >>delete_monkey(2, "comma_cleanup"),
    #likewise, this is somewhat too forgiving.
    #some commas before } or ] should cause syntax errors

    #turn lvalues into rvalues if not followed by an assignop
    -[-{:lvalue =>true}, (Op('=',true)|MODIFYASSIGNOP|LHS_COMMA).~.la]>>
      stack_monkey("lval2rval",2,-{:lvalue =>nil}){|stack| 
         stack[-2].lvalue=nil
      },
    
    #expand the = into a separate token in calls to settors (after . or ::).
    #but not in method headers
    -[(OPERATORLIKE_LB&~KW('def')).lb, Expr, DotOp|DoubleColonOp, 
      (MethNameToken&-{:has_equals=>true}).la]>>
      stack_monkey("expand_equals",1,CallNode){|stack| 
        methname=stack.pop
        methname.ident.chomp!('=')
        offset=methname.offset+methname.ident.size
        stack.push(
          CallNode.new(methname,nil,nil,nil,nil),
          OperatorToken.new('=',offset)
        )
      },

   -[NumberToken|SymbolToken]>>LiteralNode,

   #lexer does the wrong thing with -22**44.5, making the - part
   #of the first number token. it's actually lower precedence than
   #**... this rule fixes that problem.
   #in theory, unary - is lower precedence than ., ::, and [] as well, but
   #that appears not to apply to unary - in numeric tokens
   -[NumberToken&-{:negative=>true}, Op('**').la]>>
      stack_monkey("fix_neg_exp",2,Op("-@",true)){|stack|
        #neg_op.unary=true
        num=stack[-2]
        op=OperatorToken.new("-@",num.offset)
#        op.startline=num.startline
        stack[-2,0]=op
        num.ident.sub!(/\A-/,'')
        num.offset+=1
      },
 
   #treat these keywords like (rvalue) variables.
   -[@varlikes]>>VarLikeNode,

   #here docs
   -[HerePlaceholderToken]>>HereDocNode,
   #-[HereBodyToken.la]>>delete_monkey(1,"delete_here_body"),   ##this is rediculous. this should be a lexer hack?

   -[VarNameToken]>>VarNode,


  ]

  if @rubyversion >= 1.9
    result.concat [
#      -['->', ParenedNode.-, 'do', Expr.-, 'end']>>ProcLiteralNode,
#      -['->', VarLikeNode["nil",{:@value=>nil}].reg, 'do', Expr.-, 'end']>>ProcLiteralNode,
      -[(DotOp|DoubleColonOp).lb, '(',Expr.-,')', BlockNode.-, KW('do').~.la]>>CallNode,
     ]
  end

  return result
  end

if defined? END_ATTACK
  module Reducer; end
  include Reducer
end

  def signature
    ancs=class<<self; ancestors end.reject{|k| !k.name}
    ancs.slice!(ancs.index(RedParse)..-1)

    lancs=class<<@lexer; ancestors end.reject{|k| !k.name}
    [RubyLexer,Array].each{|k|
      if i=lancs.index(k)
        lancs.slice!(i..-1)
      end
    }

    RedParse.signature(@encoding,ancs+lancs)
  end
  def RedParse.signature(encoding,ancs=ancestors)
    @@my_sha||=Digest::SHA256.file(__FILE__)
    @@node_sha||=Digest::SHA256.file(__FILE__.sub(/\.rb\z/,"/node.rb"))
    [ancs.map{|m| m.name}, encoding, @@my_sha, @@node_sha,]
  end

  def initialize(input,name=nil,line=nil,lvars=nil,options=nil)
    line,name=name,nil if Hash===name or Array===name or Integer===name
    lvars,line=line,nil if Hash===line or Array===line
    options,lvars=lvars,nil if Hash===lvars
    options||={}
    name||=options[:name]||"(eval)"
    line||=options[:line]||1
    lvars||=options[:lvars]||[]
    @rubyversion=options[:rubyversion]||1.8
    input.binmode if input.respond_to? :binmode

    @encoding=options[:encoding]||:ascii
    @encoding=:binary if @rubyversion<=1.8

    if Array===input
      def input.get1token; shift end
      @lexer=input
      if @rubyversion>=1.9
        @funclikes=RubyLexer::RubyLexer1_9::FUNCLIKE_KEYWORDS
        @varlikes=RubyLexer::RubyLexer1_9::VARLIKE_KEYWORDS
      else
        @funclikes=RubyLexer::FUNCLIKE_KEYWORDS
        @varlikes=RubyLexer::VARLIKE_KEYWORDS
      end
    else
      if RubyLexer===input
        @lexer=input
      else
        @lexer=RubyLexer.new(name,input,line,0,:rubyversion=>@rubyversion,:encoding=>@encoding)
      end
      @funclikes=@lexer::FUNCLIKE_KEYWORDS()
      @varlikes=@lexer::VARLIKE_KEYWORDS()
      lvars.each{|lvar| @lexer.localvars[lvar]=true }
      @encoding=@lexer.encoding_name_normalize(@encoding.to_s).to_sym
      warn "#{@encoding} encoding won't really work right now" if RubyLexer::NONWORKING_ENCODINGS.include? @encoding
    end
    @funclikes=/#@funclikes|^->$/ if @rubyversion>=1.9
    @filename=name
    @min_sizes={}
    @compiled_rules={}
    @moretokens=[]
    @unary_or_binary_op=/^[-+]$/
#    @rules=self.expaneded_RULES
    @precedence=self.PRECEDENCE
    @RIGHT_ASSOCIATIVE=self.RIGHT_ASSOCIATIVE
if defined? END_ATTACK
    compile
end
    @saw_item_that=nil
    @print_filter=proc{true}
 
    if modules=options[:modules]
      modules.each{|m| extend m}
    end
    if modules=options[:lexer_modules]
      modules.each{|m| @lexer.extend m}
    end

    dir=reduce_withs_directory
    modname="ReduceWithsFor_#{parser_identity.join('_').tr(':.','_')}"

    cache=Cache.new(
      File===input,name, 
        :line,line,:encoding,@encoding,:locals,lvars.sort.join(","), 
      @rubyversion, :/, *signature
    )
    cache_mode=options[:cache_mode]||ENV['REDPARSE_CACHE']||:read_write
    cache_mode=cache_mode.to_sym
    raise ArgumentError,"bad cache mode #{cache_mode}" unless /^(?:read_(?:write|only)|write_only|none)$/===cache_mode.to_s    
    read_cache= /read/===cache_mode.to_s
    if read_cache and cache and result=cache.get(input)
      @cached_result=result
      @write_cache=nil
      return
    end
    if /write/===cache_mode.to_s
      @write_cache,@input= cache,input 
    else
      @write_cache=nil
    end

    #but, need to skip warning lines matching this regexp:
    #  /(^|[/\\])#{modname}\.rb:\d+: warning: mismatched indentations at 'end' with 'if' at \d+$/

    begin
    require File.join(dir,modname)
    rescue LoadError
    else
     extend self.class.const_get( modname )
    end
    redparse_modules_init
  end

  def self.parse(*args)
    new(*args).parse
  end

  def redparse_modules_init

  end

  def reduce_withs_directory
    "redparse"
  end

  attr_accessor :lexer, :print_filter
  attr_reader :rubyversion, :stack

  alias rules expanded_RULES

  def get_token(recursing=false)
    unless @moretokens.empty? 
      @last_token=@moretokens.shift
      p @last_token if ENV['PRINT_TOKENS'] && @print_filter[@last_token] and not recursing
      return @last_token
    end

    rpt=ENV['RAW_PRINT_TOKENS']
    begin
      result=@lexer.get1token or break
      p result if rpt and @print_filter[result]

      #set token's line
      result.startline= @endline||=1
      #result.endline||=@endline if result.respond_to? :endline=

      if result.respond_to?(:as) and as=result.as
        #result=make_kw(as,result.offset)
        #result.originally=result.ident
        if OperatorToken===result #or KeywordToken===result
          result=result.dup
          result.ident=as
        else
          result2=make_kw(as,result.offset)
          result2.startline=result.startline
          result2.endline=result.endline
          result=result2
        end
        result.not_real! if result.respond_to? :not_real!
      else

      case result
      when FileAndLineToken #so __FILE__ and __LINE__ can know what their values are
        @file=result.file
        @endline=result.line
        redo
      
      when OperatorToken
        if @unary_or_binary_op===result.ident and result.unary || result.tag==:unary
          result=result.dup
          result.ident+="@"
        end

      #more symbol table maintenance....
      when KeywordToken
          case name=result.ident

          when /^(#{BINOP_KEYWORDS.join '|'})$/o #should be like this in rubylexer
            unless result.has_end?
              orig=result 
              result=OperatorToken.new(name,result.offset)
              result.endline=orig.endline
            end
          when "|";
            orig=result 
            result=GoalPostToken.new(result.offset) #is this needed still? (yes)
            result.endline=orig.endline
          when "__FILE__"; #I wish rubylexer would handle this
            #class<<result; attr_accessor :value; end
            assert result.value==@file.dup
          when "__LINE__"; #I wish rubylexer would handle this
            #class<<result; attr_accessor :value; end
            assert result.value==@endline
          else 
            result=make_kw name,result.offset if defined? SPECIALIZED_KEYWORDS
            #warning, this may discard information stored in instance vars of result
          end

      when StringToken,HerePlaceholderToken
        @endline=result.endline

      when EoiToken; break
      when HereBodyToken;
        @endline=result.endline
        redo
      when AssignmentRhsListStartToken; break
      when AssignmentRhsListEndToken; break
      when IgnoreToken; redo
      end
      end
    end while false
    p result if ENV['PRINT_TOKENS'] && @print_filter[@last_token] unless recursing

    #ugly weak assertion
    assert result.endline==@endline unless result.ident==';' && result.endline-1==@endline or EoiToken===result

    return @last_token=result
  end

  def unget_tokens(*tokens)
    @moretokens=tokens.concat @moretokens
  end

  def unget_token(token)
    @moretokens.unshift token
  end

end


if __FILE__==$0
  #this code has moved to bin/redparse; really, all this should just go away
  require 'problemfiles'
  class NeverExecThis<RuntimeError; end

  def arraydiff(a,b)
    a==b and return [a,false]
    (Array===a or a=[a])
    result= a.dup
    diff=false
    size= a.size >= b.size ? a.size : b.size
    size.times{|i|
      ai=a[i]
      bi=b[i]
      if Array===ai and Array===bi
        result_i,diff_i= arraydiff(ai,bi)
        diff||=diff_i
        result[i]=result_i
      elsif ai!=bi
        next if Regexp===ai and ai.to_s==bi.to_s and 
          ai.options==bi.options 
        diff=true
        result[i]={ai=>bi}
      elsif ai.nil? 
        result[i]={'size mismatch'=>"#{a.size} for #{b.size}"} if a.size!=b.size
        diff=true
      end
      if i.nonzero? and Hash===result[i] and Hash===result[i-1]
        old=result[i-1]
        oldkeys=old.keys
        oldvals=old.values
        if Reg::Subseq===oldkeys.first
          oldkeys=oldkeys.children
          oldval=oldvals.children
        end
        result[i-1..i]=[ {-[*oldkeys+result[i].keys]=>-[*oldvals+result[i].values]} ]
      end
    }
    return result,diff
  end

    output=:pp
    quiet=true
  while /^-/===ARGV.first
    case opt=ARGV.shift
      when "--"; break
      when "--pp"; output=:pp
      when "--lisp"; output=:lisp
      when "--parsetree"; output=:parsetree
      when "--vsparsetree"; output=:vsparsetree
      when "--vsparsetree2"; output=:vsparsetree2
      when "--update-problemfiles"; problemfiles=ProblemFiles.new
      when "-q"; quiet=true
      when "-v"; quiet=false
      when "-e"; inputs=[ARGV.join(" ")]; names=["-e"]; break
      else fail "unknown option: #{opt}"

    end
  end
  
  unless inputs
    if ARGV.empty?
      inputs=[STDIN.read]
      names=["-"]
    elsif ARGV.size==1 and (Dir.entries(ARGV.first) rescue false)
      names=Dir[ARGV.first+"/**/*.rb"]
    else
      names=ARGV.dup
    end
    inputs||=names.map{|name| File.open(name).read rescue nil}
  end

  result=0
 
  safety="BEGIN{raise NeverExecThis};BEGIN{throw :never_exec_this,1};\n"
  nullsafety="\n"
  safe_inputs=inputs.map{|input| safety+input}

  inputs.each_index{|i| 
    begin

    input=inputs[i] or next
    name=names[i]

    input=nullsafety+input
    #print name+"... "; STDOUT.flush

    begin
      tree=nil
      if catch(:never_exec_this){
        tree=RedParse.new(input,name).parse; nil
      } #raise NeverExecThis
#    rescue RedParse::ParseError=>e
#      require 'pp'
#      pp e.stack[-[15,e.stack.size].min..-1]
#      raise
#    rescue NeverExecThis
        puts "RedParse attempted to execute parse data in #{name}"
        next
      end
    rescue Interrupt; exit 2
    rescue Exception=>e
#      puts e.backtrace.join("\n")
      e.message << " during parse of #{name}"
#      err=e.class.new(e.message+" during parse of #{name}")
#      err.set_backtrace e.backtrace
      problemfiles.push name if problemfiles
      raise e
    end
    tree or fail "parsetree was nil for #{name}"

    case output
    when :pp
      require 'pp'
      pp tree
    when :lisp
      puts tree.to_lisp
    when :parsetree
      pp tree.to_parsetree
    when :vsparsetree,:vsparsetree2
      begin
      require 'rubygems'
      rescue Exception
      end
      require 'parse_tree'
      #require 'algorithm/diff'
      begin
        mine=tree.to_parsetree(:quirks)
        if IO===input 
          input.rewind
          input=input.read
        end
        ryans=nil
        catch(:never_exec_this){
          ryans=ParseTree.new.parse_tree_for_string(safe_inputs[i],name); nil
        } and raise NeverExecThis
        delta,is_diff=arraydiff(mine,ryans)
      rescue NeverExecThis
        puts "ParseTree attempted to execute parse data in #{name}"
        next
      rescue Interrupt; exit 2
      rescue Exception=>e
        #raise( RuntimeError.new( "#{e} during to_parsetree of #{name}" ) )
        puts "error during to_parsetree of #{name}"
        problemfiles.push name if problemfiles
        raise
      end
      if output==:vsparsetree2
        if !quiet or is_diff
          puts "mine:"
          pp mine
          puts "ryans:" if is_diff
          pp ryans if is_diff
        end
      elsif !quiet or is_diff
        puts 'differences in '+name if is_diff
        pp delta
      end
      if is_diff
        result=1
        problemfiles.push name if problemfiles
      else
        puts "no differences in "+name
        problemfiles.delete name if problemfiles
      end
    end

    rescue NeverExecThis
      puts "mysterious attempt to execute parse data in #{name}"
      next
    rescue Interrupt,SystemExit; exit 2
    rescue Exception=>e
      puts "#{e}:#{e.class}"
      puts e.backtrace.join("\n")
      #problemfiles.push name if problemfiles
      #raise
    ensure
      STDOUT.flush
    end
  }
  exit result
end

=begin old todo:
v merge DotCallNode and CallSiteNode and CallWithBlockNode
v remove actual Tokens from parse tree...
v split ParenedNode into ParenedNode + Rescue/EnsureNode
x 'incomplete' subtrees such as ElseNode, ElsifNode, RescueNode 
x -should not appear in final output
v split keywordopnode into loop and if varieties?
=end

=begin old optimization opportunities:, ha!
top of stack slot contains mostly keywords, specific node classes, and Expr
lookahead slot contains mostly lower_op and keywords, with a few classes and inverted keywords
-(lower_op is hard to optimize)
if top of stack matcher is Expr, then the next matcher down is mostly keywords, with some operators
class membership can be optimized to test of integer within a range
keywords could be stored as symbols instead of strings
a few rules may need exploding (eg, ensure) to spoon feed the optimizer
make all Nodes descendants of Array
=end

#todo:
#each node should have a corresponding range of tokens 
#-in an (optional) array of all tokens printed by the tokenizer.
#v test stack_monkey mods
#v break ParenedNode into 2 (3?) classes
#x invent BEGINNode/ENDNode? (what other keywords?)
#v at least make BEGIN/END be KWCallNode
#v replace VarNameToken with VarNode in parser
#x convert raw rules to lists of vertex identities?
#v DottedRule class
#v ParserState class (set of DottedRules)
#v MultiReduce
#v MultiShift
#v ParserState#evolve(identity)
#v DottedRule#evolve(identity)
#v RedParse#enumerate_states
#v RedParse#enumerate_exemplars
#v Node/Token.enumerate_exemplars
#v Node/Token.identity_param
#v rename #lvalue? => #lvalue
#x likewise get rid of other oddly named identity params
#v BareMethod,WITHCOMMAS,BEGINAFTEREQUALS should have predicate methods defined for them
#v do something about BEGINAFTEREQUALS... lots predicates, ugly to identify
#v document identity parameters in nodes and tokens
#operator and keyword tokens have some identity_param variations remaining...maybe?
#xx all identity readers have to have writers as well (even if fake)
#v sort out vertex identities... call identity_param in apt classes
#convert identities<=>small ints
#convert ParserStates<=>small ints
#> lower_op/proc lookahead requires special action type with shift and reduce branches
#x stack monkeys dictate some nodes appear in s/r table... which ones?
#x some stack monkeys pushback nodes, action table must take take those as input
#v retype GoalPostNode => GoalPostToken 
#v then, pushback* should go away
#v build shift/reduce table
#v build goto table
#split tables into shift/reduce and goto....?
#v integrate with c code generator
#finish c code generator
#code generator needs a way to deal with :
  #backtracking (to more than 1 node/token???)
  #actions (stack monkeys/lower_op)
  #every reduce requires b/ting thru the lookahead
  #garbage collection
#sharing ruby objects between ruby code and generated c code
#optimizer?
#ruby code generator?
#v what to do with :shift ?
#what to do with :accept ?
#what to do with :error ?
#Node.create (used in generated code)
#Node.create <= takes input directly from semantic stack
#build Node.create param list generator
#v names for rules, dotted rules, parser states, identities
#x StartNode may be a problem... used by a stack monkey,
  #to remove extra ;s from the very beginning of input.
  #use a lexer hack instead?
#v convert StartNode to StartToken?
#convert names to numbers and numbers to names
  #for states, rules, vertex identities
  #in ruby and c (??)
#x rule for HereBodyToken should be a lexer hack?
#v stack monkeys should have names
#how to handle a stack monkey whose 2nd parameter is not a single identity?
#even reduces may not have enough info since 1 node class may have multiple identities
#v RedParse constants should be named in inspect
#v toplevel rule?
#v semantic stack in generated c code should be a ruby array
#x state stack should keep size of semantic stack at the time states are pushed, 
  #so that i can restore semantic stack to former state when b-ting/reducing
#urk, how do I know how many levels of state stack to pop when reducing?
  #in looping error rules, just scan back in semantic stack for rule start
  #in regular looping rules, transition to loop state is saved on a special stack
    #so that at reduce time, we can b/t to that point for a start
  #if rule contains only scalars, b/t is easy
  #else rule contains scalars and optionals: 
    #scan for rule start vertex starting at highest node 
    #on semantic stack that can contain it and working downward.
    #also, statically verify that relevent rules contain no collisions among first (how many?) matchers

#is lookahead in code generator even useful? my tables have built-in lookahead....
#need hack to declare nonerror looping matchers as irrevokable (for speed, when reducing)
#v assignmentRhsNode needs an identity_param for with_commas
#v -** fixup and setter breakout rules need dedicated identity_params too
# = rescue ternary is broken again now...
#v instead of shift states and is_shift_state? to find them,
  #v i should have shift transitions. (transitions that imply a shift... in response to a token input.)
  #v all states will have 2 entry points, for shift and nonshift transitions.
#split big table into goto(node) and sr(token) tables
#in each state, most common sr action should be made default
#unused entries in goto table can be ignored.
#most common goto entries (if any) can be default.
#is the change_index arg in stack_monkey calls really correct everywhere? what are
  #the exact semantics of that argument? what about stack_monkeys that change the stack size?
  #should there be another arg to keep track of that?
#maybe rewrite stack_monkeys so they're a  little clearer and easier to analyze (by hand)
#MultiShift/MultiReduce are not supported actions in generate.rb
#:accept/:error are not supported actions in generate.rb

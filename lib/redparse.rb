=begin
    redparse - a ruby parser written in ruby
    Copyright (C) 2008  Caleb Clausen

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

#warn 'hacking up LOAD_PATH to include the latest RubyLexer!'
#$:.unshift Dir.pwd+'/../rubylexer/lib', Dir.pwd+'/../rubylexer'


require 'forwardable'

begin
  require 'rubygems'
rescue LoadError=>e
  #hope we don't need it
  raise unless /rubygems/===e.message
end
require 'rubylexer'
require 'reg'

require "redparse/node"
#require "redparse/decisiontree"
require "redparse/reg_more_sugar"
require "redparse/generate"

class RedParse
  
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

    #I could call this a JIT compiler, but that's a bit grandiose....
    #more of a JIT pre-processor
    compiled_rule=@compiled_rules[rule]||=
      rule.map{|pattern| 
        String|Regexp===pattern ? KW(pattern) : pattern 
      }

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
        node=node_type.new(*matching)
        node.startline||=@stack[matchrange.first].startline
        node.endline=@endline
        @stack[matchrange]=[node]
    when Proc,StackMonkey;   node_type[@stack]
    when :shift; return 0
    when :accept,:error; throw :ParserDone
    else fail
    end
    
    return true #let caller know we found a match

    
  rescue Exception=>e
    #puts "error (#{e}) while executing rule: #{rule.inspect}"
    #puts e.backtrace.join("\n")
    raise
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
  def reduce
      shift=nil
      @rules.reverse_each{|rule|
        shift=evaluate(rule) and break
      }
      return shift
  end

  def parse
    #hack, so StringToken can know what parser its called from
    #so it can use it to parse inclusions
    oldparser=Thread.current[:$RedParse_parser]
    Thread.current[:$RedParse_parser]||=self

    @rules||=expanded_RULES()
#    @inputs||=enumerate_exemplars

    @stack=[StartToken.new, get_token] 
           #last token on @stack is always implicitly the lookahead
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

    @stack.size==2 and return NopNode.new #handle empty parse string

    #unless the @stack is 3 tokens, 
    #with the last an Eoi, and first a StartToken
    #there was a parse error
    unless @stack.size==3
      pp @stack[-[15,@stack.size].min..-1] if ENV['PRINT_STACK']
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
      not if node.respond_to? :error? and node.error?(@rubyversion)
        msgs<< @filename+":"+node.blame.msg
      end
    } if result.respond_to? :walk #hack hack
    result.errors=msgs unless msgs.empty?
    #other types of errors (lexer errors, exceptions in lexer or parser actions)
    #should be handled in the same way, but currently are not
#    puts msgs.join("\n")

=begin
  rescue Exception=>e
      input=@lexer
      if Array===input
        puts "error while parsing:"
        pp input
        input=nil
      else
        input=input.original_file
        inputname=@lexer.filename
        input.to_s.size>1000 and input=inputname
        puts "error while parsing: <<<  #{input}  >>>"
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
    @stack=nil
    Thread.current[:$RedParse_parser]=oldparser
  end


  #HIER=Class::FlattenedHierarchy.new *STACKABLE_CLASSES

  def new_disabled_reduce
    #@hier||=Class::FlattenedHierarchy.new *STACKABLE_CLASSES()
    @reducer||=Reducer.new(@rules)

    @reducer.reduce(@stack)
  end #
#
if defined? END_ATTACK
  class RuleSet
    def initialize(rules)
      @rules=rules.reverse
      #rule order must be reversed relative to the usual RedParse rule
      #order... merely so that ffs can work right.
      @maxmask=(1<<@rules.size)-1
      @subclasses_of=child_relations_among(*STACKABLE_CLASSES())
    end

    def rules2mask(rules)
      mask=0
      @rules.each_with_index{|r,i| 
         mask |= 1<<i if rules.include? r
      }
      return mask
    end

    def mask2rules(mask)
      rules=[]
      @rules.each_with_index{|r,i|
        rules<<r if mask&(1<<i)
      }
      return rules
    end

    def mask2rules(mask)
      result=[]
      while mask.nonzero?
        result<< @rules[i=ffs(mask)-1]
        mask &= ~(1<<i)
      end
      return result
    end

    def each_rule(mask=-1)
      @rules.each_with_index{|r,i|
        yield r,i if mask&(1<<i)
      }
    end

    def each_rule(mask=@maxmask)
      while mask.nonzero?
        yield @rules[i=ffs(mask)-1],i
        mask &= ~(1<<i)
      end
    end


    @@FFS_TABLE=[nil]
    1.upto(8){|n|
      @@FFS_TABLE*=2
      @@FFS_TABLE[@@FFS_TABLE.size/2]=n
    }
    def rb_ffs(mask)
      chunks=0
      until mask.zero?
        result=@@FFS_TABLE[mask&0xFF]
        return result+(chunks<<3) if result
        chunks+=1
        mask>>=8
      end
      return 0
    end

    begin
      require 'inline'
      inline{|inline|
      inline.prefix '#define _GNU_SOURCE'
      inline.include '"string.h"'
      inline.include '"limits.h"'
      inline.c %{
        unsigned c_ffs(VALUE mask){
          if FIXNUM_P(mask) {
            return ffsl(NUM2UINT(mask));
          } else if(TYPE(mask)==T_BIGNUM) {
            struct RBignum* bn=RBIGNUM(mask);
            int len=bn->len;
            int i;
            unsigned offset=0;
            unsigned result=0;
            for(i=0;i<len;++i){
              /*printf("least:%x\\n", ((BDIGIT*)(bn->digits))[i]);*/
              /*printf("most:%x\\n", ((BDIGIT*)(bn->digits))[len]);*/
              result=ffs(((BDIGIT*)(bn->digits))[i]);
              if (result) break;
              offset+=sizeof(int)*CHAR_BIT;
            }
            if (result==0) return 0;
            return result+offset;
          } else {
            rb_fatal("bad argument to ffs");
          }
        }
      }
      }
      alias ffs c_ffs
    rescue Exception=>e
      warn "error (#{e.class}) while defining inline c ffs()"
      warn "original error: #{e}"
      warn "falling back to ruby version of ffs()"
      alias ffs rb_ffs

    end

    


    #just the left side (the stack/lookahead matchers)
    def LEFT
      @rules.map{|r| r.left.subregs }.flatten
    end

    #remove lookahead and lookback decoration
    def LEFT_NO_LOOKING
    l=LEFT()
    l.map!{|m| 
      case m #
      when Reg::LookAhead,Reg::LookBack; m.subregs[0]
      when Proc; []
      else m #
      end #
    } 
    l
    end

    #all classes mentioned in rules, on left and right sides
    def STACKABLE_CLASSES #
      return @sc_result unless 	@sc_result.nil? 
      @sc_result=false
      l=LEFT_NO_LOOKING()
      l=l.map{|lm| sc_juice lm}.flatten.compact
      r=  @rules.map{|rr| rr.right }.grep(Class) #classes in productions
      result=l+r 
      @sc_result=result.grep(Class).uniq
      fail if @sc_result.empty?
      return @sc_result
    end

    def juice(m)
      case m  #
      when Class; 
        return [m] unless @subclasses_of
        result=[m]  # and subclasses too
        i=0
        while item=result[i]
          #p item
          result.concat @subclasses_of[item]
          i += 1
        end
        result
      when String,Regexp; juice(RedParse.KW(m))
      when Reg::And; m.subregs.map{|x| juice(x).flatten.compact}.inject{|sum,rr| sum&rr} 
      when Reg::Or; m.subregs.map( &method(:juice) )
      when Reg::Not;  
        m=m.subregs[0]
        if Class===m or (Reg::Or===m  and 
             m.subregs.inject{|sum,x| sum && (Class===x) })
          j=juice(m)
          STACKABLE_CLASSES()-j.flatten.compact rescue j
        else 
          STACKABLE_CLASSES()
        end
      else STACKABLE_CLASSES()
      end
    end

    def sc_juice(m) 
      case m #
      when Class; [m]
      when String,Regexp; juice(RedParse.KW(m))
#      when String,Regexp; [KeywordToken]
      when Reg::And; m.subregs.map{|x| sc_juice(x)}.compact.map{|x| x.flatten.compact}.inject{|sum,rr| sum&rr }
      when Reg::Or; m.subregs.map( &method(:sc_juice) )
      when Reg::Not; sc_juice(m.subregs[0])
      when Reg::LookAhead, Reg::LookBack; sc_juice(m.subregs[0])
      else []
      end
    end

    def LOOKAHEAD_CLASSES rule
      last=rule.left.subregs.last 
      return STACKABLE_CLASSES() unless Reg::LookAhead===last
      la= last.subregs[0]
      return juice(la).flatten.compact
    end
  #
    def TOS_CLASSES rule
      i=-1
      mats=rule.left.subregs
      m=mats[i]
      m=mats[i-=1] if Reg::LookAhead===m || Proc===m
      result=[]
      while Reg::Repeat===m and m.times.min.zero?
        result<<juice(m.subregs[0])
        m=mats[i-=1]
      end
      return (result+juice(m)).flatten.compact
    end
   
    def [](i)
      @rules[i]
    end

  end   #
#
  module Reducer
    @@rulesets={}
    @@class_narrowerses={}
    def compile(recompile=false)
      klass=self.class

      #use cached result if available
      if @@rulesets[klass] and !recompile
        @ruleset=@@rulesets[klass]
        @class_narrowers=@@class_narrowerses[klass]
        return
      end

      #actual rule compilation
      @ruleset=RuleSet.new @rules
      @class_narrowers=[tos=Hash.new(0),la=Hash.new(0)]
      @ruleset.each_rule{|r,i|
          @ruleset.LOOKAHEAD_CLASSES(r).each{|klass2|
            la[klass2] |= 1<<i
          }
          @ruleset.TOS_CLASSES(r).each{|klass2|
            tos[klass2] |= 1<<i
          }
      }

      #save result to cache if not too dynamic
      if !recompile
        @@rulesets[klass]=@ruleset
        @@class_narrowerses[klass]=@class_narrowers
      end
    end

    def new_reduce
#      mask=-1
#      (-1).downto(-@class_narrowers.size){|i|
#        mask &= @class_narrowers[i][@stack[i].class]
#      }
      mask= 
        @class_narrowers[-1][@stack[-1].class]&
        @class_narrowers[-2][@stack[-2].class]
      @ruleset.each_rule(mask){|r,i|
        res=evaluate(r) and return res
      }
      return false
    end
  end
end

  def map_with_index(list)
    result=[]
    list.each_with_index{|elem,i| result<<yield(elem,i)}
    result
  end

  def all_rules
    return @all_rules if defined? @all_rules

    @inputs||=enumerate_exemplars
    @rules=expanded_RULES  #force it to be recalculated
    @all_rules = map_with_index(@rules){|r,i| Rule.new r,i}

    @all_rules.each{|r|
      if StackMonkey===r.action
        r.action.exemplars=@inputs.grep r.action.hint
      end
    }
 
    warn "error recovery rules disabled for now; creates too many states and masks errors"
    @all_rules.reject!{|r| r.action==MisparsedNode }

    #names have to be allocated globally to make sure they don't collide
    names=@all_rules.map{|r| 
      if r.action.respond_to? :name 
        r.action.name
      else
        r.action.to_s
      end
    }.sort
    dups={}
    names.each_with_index{|name,i|
      dups[name]=0 if name==names[i+1]
    }
    @all_rules.each{|r|
      r.name=
      if r.action.respond_to? :name 
        r.action.name.dup
      else
        r.action.to_s
      end
      if dups[r.name]
        count=dups[r.name]+=1
        r.name<<"_#{count}"
      end
    }
  end

  def all_dotted_rules
    all_rules.map{|rule| 
      (0...rule.patterns.size).map{|i| 
        DottedRule.create(rule,i,self) 
      }
    }.flatten
  end

#$OLD_PAA=1

  def all_initial_dotted_rules
    return @all_initial_dotted_rules if defined? @all_initial_dotted_rules
    @all_initial_dotted_rules=result=
      all_rules.map{|rule| DottedRule.create(rule,0,nil) }

    p :all_init

unless defined? $OLD_PAA
    scanning=result
    provisionals=nil
    while true
      old_provisionals=provisionals
      provisionals={}
      scanning.each{|dr| 
        dr.also_allow=dr.compute_also_allow(provisional=[false]) #fill out dr.also_allow
        provisionals[dr]=provisional[0]
      }
      scanning=provisionals.map{|dr,val| dr if val }.compact
    end until provisionals==old_provisionals
end
    p :all_init_done

    return result
  end

  class Rule #original user rules, slightly chewed on
    def initialize(rawrule,priority)
      @priority=priority
      @action=rawrule.right
      @patterns=rawrule.left.subregs.dup
      #remove lookback decoration if any, just note that lb was present
      if Reg::LookBack===@patterns[0] 
        @lookback=true
        @patterns[0]=@patterns[0].subregs[0]
      end

      case @patterns[-1]
      #Symbol is pointless here, methinks.
      when Proc,Symbol;    #do nothing
      when Reg::LookAhead; @patterns[-1]=@patterns[-1].subregs[0]
      else                 @patterns.push Object  #add la if none was present
      end

      #search for looping matchers with minimum >0 and replace them
      #with a number of scalars (== the minimum) followed by a loop with 0 min.
      #search for bare strings or regexps and replace with KW(   ) wrapper
      @patterns.each_with_index{|p,i|
        case p
        when String,Regexp; @patterns[i]=RedParse.KW(p)
        when Reg::Repeat
          if p.itemrange.first>0
            @patterns[i,1]=
              *[p.subregs[0]]*p.itemrange.first<< #minimum # as scalars
              p.subregs[0].reg.* #0-based looper
          end
        end
      }
      @drs=[]
    end

    attr_reader :drs

    def hash; priority end
    def == other; Rule===other and priority==other.priority end
    alias eql? ==

    def lookback?; @lookback if defined? @lookback end

    attr_reader :patterns,:action,:priority
    attr_accessor :name

    def at(n)
      result=patterns[n]
      result=result.subregs[0] if Reg::Repeat===result
      result
    end
    def optional? n
      p=patterns[n]
      return Reg::Repeat===p && p.itemrange.first.zero?
    end
    def looping? n
      p=patterns[n]
      return false unless Reg::Repeat===p 
      return false if p.itemrange.last==1
      fail unless p.itemrange.last.infinite?
      return true
    rescue Exception
      return false
    end

    def reduces_to
      case @action
      when Class; @action
      when StackMonkey; @action.exemplars
      when :error,:shift,:accept; nil
      else fail "#@action unexpected in reduces_to"
      end
    end

    def unruly?
      return if action==:accept
      action.class!=Class || lookback?
    end

    def final_promised_pattern
      case @action
      when DeleteMonkey #delete_monkey
        vector_indexes=(@action.first_changed_index..-1).select{|i| Reg::Repeat===@patterns[i] }
        fail unless vector_indexes.empty?
        result=@patterns.dup
        result.delete_at @action.first_changed_index
      when StackMonkey #stack_monkey
        result=@patterns.dup
        result[@action.first_changed_index..-1]=[@action.hint]
      when Class
        result= [@action,@patterns.last]
        result.unshift @patterns.first if lookback?
      when :accept, :error, :shift
        result=@patterns.dup
      else 
        pp @action
        fail
      end
      result[-1]=result[-1].la unless result.empty?
      result
    end

    def final_promised_rule
      @final_promised_rule ||=
        Rule.new(-final_promised_pattern>>nil,-priority)
    end
  end 

  class DottedRule
    def initialize(rule,pos,parser)
      @rule,@pos=rule,pos
      fail unless (0...rule.patterns.size)===@pos
#      @also_allow= compute_also_allow(parser) if parser unless defined? $OLD_PAA
    end
    def compute_also_allow(parser,provisional=[false])
        parser.all_initial_dotted_rules.map{|dr|
          next if dr==self
          fake_rule=dr.rule.final_promised_rule
          final_more_dr=DottedRule.create(fake_rule,0,nil)
          also=dr.also_allow
          unless also
            provisional[0]||=0
            provisional[0]+=1
            also=[]
          end
          also+[dr] if optionally_combine final_more_dr,parser
        }.flatten.compact.uniq
    end
    attr_reader :rule,:pos
    attr_accessor :also_allow

    def self.create(rule,pos,parser)
      result=rule.drs[pos] and return result
      result=rule.drs[pos]=DottedRule.new(rule,pos,parser)
unless defined? $OLD_PAA
      result.also_allow=result.compute_also_allow(parser) if parser
end
      return result
    end

    def hash; (@rule.priority<<3)^@pos end
    def == other; DottedRule===other and @pos==other.pos and @rule==other.rule end
    alias eql? ==

    def name; @rule.name+"@#@pos"  end

    def looping?
      @rule.looping?(@pos)
    end

    #returns Conditional|Rule|DottedRule|+[DottedRule.+]|nil
    def evolve input, parser, seenlist,result2
      #print "["
      #$stdout.flush
      idname=input.identity_name
      idname=parser.identity_name_alias? idname 
      cache=seenlist[[self,idname]]
      unless cache==:dunno_yet
        result2.concat Array(cache).flatten.compact.uniq.sort_by{|x| x.name}
        return cache
      end
      i=pos
      lasti=i-1
      result=[]
      result=loop do #might need multiple tries if optional matcher(s) here
        fail unless i>lasti
        lasti=i
        p=@rule.at(i)  #what is current pattern in this dottedrule?
        fail if Proc===p #shouldnt happen anymore
        if parser.pattern_matches_nodes? p

          #if any dotted rules have nodes at this point, 
          #also include the set of rules@0 which
          #can (possibly indirectly) generate that node.
          #(match tokens found on left sides of productions for p)
          seenlist[[self,idname]]=result
if false
          result.concat recurse_match_drs(parser).uniq.map{|dr|
            dr and 
            #begin  print "{#{dr.name}"
                   dr.evolve input,parser,seenlist,result2
            #ensure print "}" end
          }.flatten.compact.uniq
end
        end
        @saw_item_that={}
        if p===input
          i+=1 unless @rule.looping?(i)
          fail if i>@rule.patterns.size

          if !@saw_item_that.empty?
            p(:saw_item_that!)
            fail unless @saw_item_that.size==1
            pair=@saw_item_that.to_a.first
            fail unless p.equal? pair.last
            it=pair.first
            action=
            if i==@rule.patterns.size
              @rule
            else
              DottedRule.create(@rule,i,parser)
            end
            break Conditional.new(it,action)
          end
          @saw_item_that=nil

          if i == @rule.patterns.size
            break @rule
          else
            break result<<DottedRule.create(@rule,i,parser)
          end
        elsif !@rule.optional?(i) 
          break result.empty? ? nil : result
        elsif (i+=1) >= @rule.patterns.size
          break @rule
        #else next p
        end
      end #loop
      seenlist[[self,idname]]=result
      result2.concat Array(result).flatten.compact.uniq.sort_by{|x| x.name}
      return result
    #ensure print "]"
    end

    #returns +[(DottedRule|nil).*]
    def recurse_match_drs parser, result=nil
      unless result
        table=parser.rmd_cache
        if table
          cache=table[self]
          return cache if cache
        else
          parser.rmd_cache={}
        end

        result=[]
      end
      #print "("
      #print @rule.name+"@#@pos"
      p=@rule.at(@pos)
      
      #find set of nodes that could match here
      nodes_here=parser.exemplars_that_match(p&Node)

      #find the set of rules that could generate a node in our list
      rrules=parser.all_rules.select{|rule| 
               !rule.unruly? and !nodes_here.grep(rule.action).empty?
             }.map{|rule|
               DottedRule.create(rule,0,parser) 
             }

      #if any generating rules match a node in the leftmost pattern,
      #add the rules which can generate _that_ node too.
      result.push self  #force self to be excluded from future recursion
      oldsize=result.size
      unless rrules.empty?
        result.concat rrules
        
        unless result.respond_to? :index_of
          class<<result
            attr_accessor :index_of
          end
          result.index_of={}
        end
        rio=result.index_of
        oldsize.upto(result.size){|i| rio[result[i]]||=i }
        rrules.each{|rrule|
          i=rio[rrule] or fail #index() inside each() == O(N**2) complexity. this is the slow line.
          #but skip recursion on rules already done at a higher level
          rrule.recurse_match_drs parser,result if i>=oldsize
        }
      end
      result[oldsize-1]=nil #don't actually include self in result
      #result.update_indices oldsize-1, oldsize-1
 
      parser.rmd_cache[self]=result
      return result
    #ensure print ")"
    end

    def optionally_combine weaker,parser
      #lotsa caching needed if this is ever to be performant
      if parser.oc_cache
        result=parser.oc_cache[[self,weaker]]  
        return result unless result.nil?
      else
        parser.oc_cache={}
      end

      other=weaker
      mymatches,myposes=  self.outcomes
      matches,  poses  = other.outcomes
      matches.each_with_index{|match,i|
        mymatches.each_with_index{|mymatch,myi|
          intersect=parser.inputs.grep(match&mymatch)
          unless intersect.empty?

            #but don't allow matches that would be matched 
            #by an earlier (but optional) pattern.
            disallowed=Reg::Or.new(
              *possible_matchers_til(myi)+
                other.possible_matchers_til(i)
            )
            intersect.reject{|x| disallowed===x }

            if intersect.empty?
              return result=false 
            elsif poses[i]>=other.rule.patterns.size
              return result=true  #success if weaker rule is at an end
            elsif myposes[myi]>=rule.patterns.size
              return result=false             #fail if stronger rule at an end
            else
              p [:**,rule.name,myposes[myi]]
              mynew=DottedRule.create(rule,myposes[myi],parser)
              new=DottedRule.create(other.rule,poses[i],parser)
              return result=mynew.optionally_combine( new,parser )
            end
          end
        }
      }
      return result=false
    ensure
      parser.oc_cache[[self,weaker]]=result
    end

    def possible_matchers_til i
      (pos...i-1).map{|j|
        m=rule.at(j)
        Reg::Repeat===m ? m.subregs[0] : m
      }
    end

    def outcomes
      til=@rule.patterns.size
      at=@pos
      result=[[],[]]
      loop do
        m=@rule.patterns[at]
        case m
        when Proc; 
          result.first.push Object
          result.last.push at+1
          break
        when Reg::Repeat
          assert @rule.optional?(at)
          to=at
          to+=1 unless @rule.looping? at
          result.first.push m.subregs[0]
          result.last.push to
        else
          result.first.push m
          result.last.push at+1
          break
        end
        at+=1
        break if at>=til
      end
      return result
    end

  end

  attr_accessor :rmd_cache
  attr_accessor :oc_cache
  attr_accessor :sl2ms_cache

  class Conditional
    def initialize(condition,action)
      @condition,@action=condition,action
      @condition.restore :hash,:==
    end
    attr_reader :condition,:action

    def hash
      @condition.hash^@action.hash
    end
    def == other
      Conditional===other and @condition==other.condition and @action==other.action
    end
    alias eql? ==

    def name; @condition.inspect+"?"+@action.name end

    def priority; @action.priority end
  end

  class ParserState; end
  class MultiShift; end
  class MultiReduce; end

  ACTION_PATTERN=ParserState|Rule|MultiShift|MultiReduce|:accept|:error
  class ParserState #a union of dotted rules
    def initialize(dotteds,index)
      fail if dotteds.empty? #error state
      fail unless dotteds.grep(nil).empty?
      @dotteds=dotteds
      @index=index
      sort_substates!
      @actions={} #key is an input, value is ParserState|Rule|MultiShift|MultiReduce|:accept|:error
    end
   
    attr_reader :actions

    def [](k) 
      result=@actions[k]
      assert ACTION_PATTERN===result
      result
    end
    def []=(k,v) 
      assert ACTION_PATTERN===v
      @actions[k]=v 
    end

    def sort_substates!
      @dotteds=@dotteds.sort_by{|dotted| -dotted.pos}.uniq
    end
    attr :dotteds

    def dup
      result=super
      result.instance_variable_set(:@dotteds,@dotteds.dup)
      return result
    end

    def substates; [self] end

    def shiftlist2multishift? shiftlist,parser
      return :error if shiftlist.empty?
      parser.sl2ms_cache||={}
      cache=parser.sl2ms_cache[shiftlist]
      return cache if cache
      fixed,varying=shiftlist.partition{|res| DottedRule===res}
      result=ParserState.new(fixed,nil)
      result.perhaps_also_allow parser.all_rules,parser
      unless varying.empty? #MultiShift
        varying.map!{|v| [v.condition,v.action]}.flatten
        result=MultiShift.new(result,varying)
      end
      parser.sl2ms_cache[shiftlist]=result
      return result
    end

    #given a list of rules, see if any of them are compatible with
    #a current substate. (compatibility means the aggregate patterns
    #can be anded together and still be able to conceivably match something.)
    #if any of morerules are actually compatible, add it to current state.
    def perhaps_also_allow(morerules,parser)
      fail unless morerules==parser.all_rules
      @dotteds.concat @dotteds.map{|d| d.also_allow }.flatten.compact.uniq
      sort_substates!
    end
    def old_perhaps_also_allow(morerules,parser)
      morerules=morerules.dup
      need_sort=false
      scan_rules=@dotteds
      added={}
      while true
        adding=[]
        morerules.each{|morerule|
          next if added[morerule]
          fake_rule=morerule.final_promised_rule
          final_more_dr=DottedRule.create(fake_rule,0,parser)
          scan_rules.each{|dotted|
            if dotted.optionally_combine final_more_dr,parser
              adding<<DottedRule.create(morerule,0,parser)
              added[morerule]=1
              break
            end
          }
        }
        break if adding.empty?
        @dotteds.concat adding
        need_sort=true
        scan_rules=adding
      end
      sort_substates! if need_sort
    end
    alias perhaps_also_allow old_perhaps_also_allow if defined? $OLD_PAA


    #returns ParserState|MultiShift|MultiReduce|Rule|:accept|:error
    def evolve input,parser,seenlist
      result2=[]
      @dotteds.each{|dotted| 
        dotted.evolve input,parser,seenlist,result2
      }

      result= 
        #seenlist.values.flatten.compact.uniq.sort_by{|x| x.name}
      result2=result2.uniq.compact.sort_by{|x| x.name}
      #pp [result,result2].map{|x| x.map{|res| DottedRule===res ? res.name : res }}
      #pp result2.map{|res| DottedRule===res ? res.name : res }
#      result==result2 or fail

      return result=:error if result.empty?


      #ok, who wants to shift and who wants to reduce?
      shiftlist,reducelist=result.partition{|res|
        DottedRule===res or
          Conditional===res && DottedRule===res.action
      }

      #if no reducers at all, just try (multi?)shift
      return result=shiftlist2multishift?( shiftlist,parser )if reducelist.empty?

      #line up reducers by priority
      actions=reducelist \
        .sort_by{|rule| -rule.priority }
#        .map{|rule| rule.action }
      #actions is +[(Rule|Conditional[Rule]).*]
      action=actions.shift #this first (unless conditional)
      #action is Rule|Conditional[Rule]
      result=
      case action.action
      when :error; return :error
      when Class, StackMonkey
        action
      when :accept
        :accept
      when :shift #this counts as a reduce at this point, but it writes shift instructions
        shiftlist2multishift? shiftlist,parser
      when Rule #oy, vey, was a Conditional
        shiftaction=shiftlist2multishift?(shiftlist,parser)
        fail unless Rule===action.action
        case action.action.action
        when :error; huh
        when :shift, StackMonkey, :accept, Class #MultiReduce
          first_fixed_index=actions.size
          #actions is +[(Rule|Conditional[Rule]).*]
          actions.each_with_index{|act,i|
            break first_fixed_index=i unless Conditional===act
          }
          condactions=actions[0...first_fixed_index].unshift(action)
          condactions=condactions.inject([]){|sum,cond|
            act=cond.action
            act=shiftaction if act==:shift #=>shiftlist?
            sum.push cond.condition, act
          }
          #possible optimization: one or more :shift right at end could be ignored
          if actions[first_fixed_index]
            action=actions[first_fixed_index].action
          else
            action=shiftaction
          end
          MultiReduce.new condactions,action #=>shiftlist?
        else fail
        end
      else fail "#{action} not expected here"
      end
      #stack monkeys/:accept are treated like reduce here
    ensure
      assert ACTION_PATTERN===result
    end

    def name
      @name||@dotteds.map{|dotted| dotted.name}.join(",")
    end
    attr_writer :name

    def rename(name2count)
      return @name if defined? @name
      name=most_prominent_members.map{|dotted| dotted.name}.join(",")
      if name2count[name]
        name2count[name]+=1
        name+="___"+name2count[name].to_s
      else
        name2count[name]=1
      end

      @name=name
    end
    
    def most_prominent_members
      result=@dotteds.select{|dr| dr.pos==@dotteds.first.pos }
      close2end=@dotteds.map{|dr| [dr,dr.rule.patterns.size-dr.pos]}.sort_by{|(o,k)| -k}
      result+=close2end.select{|(dr,k)| k==close2end.first.last}.map{|(dr,k)| dr}
      result2=result.reject{|dr| dr.pos==0 or dr.pos==1&&dr.rule.lookback?}
      result=result2 unless result2.empty?
      return result
    end

    def hash
      -@dotteds.hash
    end
    def == other
      ParserState===other and 
        @dotteds==other.dotteds 
    end
    alias eql? ==

    def looping?
      @dotteds.any?{|dotted| dotted.looping? }
    end

    def transition_to_loop? input #not used
      action=@actions.input
      case action
      when :error; false
      when ParserState; action.looping? and action!=self
      when MultiShift,MultiReduce;
        action.transition_to_loop? input
      else fail
      end
    end

    def make_sr_goto_tables
      name2exemplar={}
      @inputs.each{|i| name2exemplar[i.name]=i }

      @goto={}; @sr={}
      goto_counts=Hash.new(0); sr_counts=Hash.new(0)
      actions.each_pair{|k,v| 
        if Node===name2exemplar[k]
          @goto[k]=v
          goto_counts[v]+=1
        else
          assert(Token===name2exemplar[k])
          @sr[k]=v
          sr_counts[v]+=1
        end
      }
      dflt=goto_counts.sort_by{|v,c| c}.last[0]
      @goto.delete_if{|k,v| v==dflt }
      @goto.default=dflt

      dflt=sr_counts.sort_by{|v,c| c}.last[0]
      @sr.delete_if{|k,v| v==dflt }
      @sr.default=dflt

      @actions=nil
    end

  end

  class MultiReduce
    def initialize(list,default)
      @list,@default=list,default
      #default can be any valid action (except another MultiReduce)
    end

    attr_reader :list,:default

    def act(x)
      (0...@list.size).step(2){|i|
        return @list[i+1] if @list[i]===x
      }
      return default
    end

    def substates
      if @default.respond_to? :substates
        @default.substates
      else
        []
      end
    end

    def actions
      result=[]
      (1...@list.size).step(2){|i|
        result << @list[i]
      }
      if @default.respond_to? :actions
        result.concat @default.actions 
      elsif @default
        result<<@default
      end
      result
    end

    def transition_to_loop? input #not used
      @default.transition_to_loop? input
    end

    def hash
      @list.hash^~@default.hash
    end

    def == other
      @list==other.list and @default==other.default
    end
    alias eql? ==
  end

  class MultiShift
    def initialize(base,modifiers)
      @base,@modifiers=base,modifiers
      @map=
        (0...2**(modifiers.size/2)).map{|i| base.dup}
      @map.each_with_index{|state,i| #for each branch to the multishift
        (0...modifiers.size).step(2){|j| #for each predicate in the multishift
          if (i&(1<<j)).non_zero? #if the predicate tests true in this branch
            state.append modifiers[j+1] #add the predicates modifier to the state
          end
        }
        state.sort_substates!
      }
    end

    def act(x)
      result=0
      (0...@modifiers.size).step(2){|i|
        result|=(1<<(i/2)) if @modifiers[i]===x
      }
      @map[result]
    end

    attr_reader :map, :modifiers

    def substates
      @map.dup
    end

    def actions
      @map.dup
    end

    def transition_to_loop? input #not used
      huh
    end

    def hash
      huh
    end
    def == other
      huh
    end
    alias eql? ==
  end

  #an action is one of:
  #a ParserState (shift)
  #a Rule (reduce)
  #nil (error)
  #:accept
  #MultiReduce
  #MultiShift

    #just the left side (the stack/lookahead matchers)
  def LEFT
#      require 'md5'
      @rules=expanded_RULES()
#      p MD5.new(@rules).to_s
      @rules.map{|r| r.left.subregs }.flatten
  end

    #remove lookahead and lookback decoration (not used?)
  def LEFT_NO_LOOKING
    l=LEFT()
    l.map!{|m|
      case m #
      when Reg::LookAhead,Reg::LookBack; fail #should be gone already now
      when Proc; []
      else m #
      end #
    }
    l
  end

  def child_relations_among(*classes)
      classes.unshift Object
      result={}
      classes.each{|klass| result[klass]=[] }

      #p classes
      classes.each{|klass|
        anclist=klass.ancestors
        anclist.shift==klass or fail
        anclist.each{|anc|
          if anc=result[anc]
            anc << klass
            break
          end
        }
      }

      return result
  end

  #all classes mentioned in rules, on left and right sides
  def STACKABLE_CLASSES #
      return @sc_result if defined? @sc_result
      @sc_result=[]
      @subclasses_of=child_relations_among(*vertices)
#      @sc_result=false
      l=LEFT()
      l=l.map{|lm| sc_juice lm}.flatten.compact
      assert l.grep(nil).empty?
      r=  @rules.map{|rr| rr.right }.grep(Class) #classes in productions
      result=l+r
      @subclasses_of=nil
      @sc_result.replace result.grep(Class).uniq
     fail if @sc_result.empty?
      return @sc_result
  end

#    def juice(m)
#      case m  #
#      when Class
#        return [m] unless @subclasses_of
#        result=[m]  # and subclasses too
#        i=0
#        while item=result[i]
#          p item
#          result.concat @subclasses_of[item] rescue nil
#          i += 1
#        end
#        result
#      when String,Regexp; juice(RedParse.KW(m))
#      when Reg::And; m.subregs.map{|x| juice(x).flatten.compact}.inject{|sum,rr| sum&rr}
#      when Reg::Or; m.subregs.map &method(:juice)
#      when Reg::Not
#        m=m.subregs[0]
#        if Class===m or (Reg::Or===m  and
#             m.subregs.find{|x| Class===x })
#          juice(m)
#        else []
#        end
#      else []
#      end
#    end

  def sc_juice(m)
      case m #
      when Class; [m]
      when String,Regexp; [KeywordToken]
      when Reg::And; m.subregs.map{|x| sc_juice(x)}.compact.map{|x| x.flatten.compact}.inject{|sum,rr| sum&rr }
      when Reg::Or; m.subregs.map(&method(:sc_juice))
      when Reg::Not; sc_juice(m.subregs[0])
      when Reg::LookAhead, Reg::LookBack; sc_juice(m.subregs[0])
      when Reg::Repeat; sc_juice(m.subregs[0])
      else []
      end
  end

  def unruly_rules
    return @unruly_rules if defined? @unruly_rules

    @unruly_rules=
      all_rules.select{|rule| rule.unruly? }

    p :unruly_rules
    pp @unruly_rules.map{|r| r.name}

    @unruly_rules
  end

  def enumerate_exemplars
    return @@exemplars if defined? @@exemplars #dunno why this is necessary

    result= STACKABLE_CLASSES() \
      .map{|sc| sc.enumerate_exemplars } \
      .inject{|sum,sc| sum+sc}

    result.map!{|sc|
        res=sc.shift.allocate
        until sc.empty?
          eval "def res.#{sc.shift}; #{sc.shift.inspect} end"
        end
        def res.to_s; identity_name end
        res
    }

    return @@exemplars=result
  end

  def check_for_parsealike_inputs
    all_patterns=all_rules.map{|r| r.patterns.map{|rp| Reg::Repeat===rp and rp=rp.subregs[0]; rp }}.flatten.uniq
    seen={}
    @identity_name_aliases={}
    warn "why are non_empty and after_equals params to BeginNode appearently ignored?"
    warn "some token identities overlap themselves?!?"
    warn "some overlaps are duplicated"
    warn ". and :: overlap => ..... surely that's not right"
    @inputs.map{|input|
      profile=all_patterns.map{|pat| Proc===pat ? pat : !!(pat===input)}
      if seen[profile]
        puts "#{input} overlaps #{seen[profile]}"
        @identity_name_aliases[seen[profile]]=input
        nil
      else
        seen[profile]=input
      end
    }.compact
  end

  def enumerate_states
    inputs=check_for_parsealike_inputs
    inputs.reject!{|x| StartToken===x}

    result=[]
    todo=[start_state]

    seenlist = {}
    seenlist.default=:dunno_yet

    j=0
    start=was=Time.now
    in_result={}  #this should go away; obsoleted by @states
    state_num=-1
    todo.each{|st| in_result[st]=(state_num+=1) }
    ps=todo.first
    pp [-in_result[ps], *ps.dotteds.map{|dr| dr.name }]
    old_todo_size=todo.size
    while state=todo.shift
      result<<state

      i=0
      inputs.each {|input|
        newstate=state.evolve input,self,seenlist
        assert ACTION_PATTERN===newstate
        #newstate is ParserState|MultiShift|MultiReduce|Rule|:accept|:error
        state[input.identity_name]=newstate
        next unless newstate.respond_to? :substates 
        #newstate.substates is just [newstate] for plain ParserStates
        morestates=newstate.substates.reject{|x| in_result[x]}
        morestates.each{|st| in_result[st]=(state_num+=1) }
#        p [in_result[state],:+,input.identity_name,:>>,pretty(newstate,in_result)]
        todo.concat morestates

#        pp morestates.map{|ps| 
#          [-in_result[ps], *ps.dotteds.map{|dr| dr.name }]
#        }
#        pp pretty(newstate,in_result) unless ParserState===newstate
      }

      now=Time.now
      p [:*,j+=1,todo.size,todo.size-old_todo_size,now-was,j/(now-start),(100.0*j/(j+todo.size)).to_i]
      old_todo_size=todo.size
      was=now

#      if state.actions.values.uniq==[:error]
         #this can happen when the only dotted rule is for an :error
         #maybe this case can be optimized?
#      end
    end
    self.rmd_cache=nil
    self.oc_cache=nil
    self.sl2ms_cache=nil
    return result
  end

    def pretty(x,in_result)
      case x
      when ParserState; in_result[x]
      when MultiReduce
        pairs=x.list.dup
        result=[]
        until pairs.empty?
          cond,act,*pairs=*pairs
          cond = cond.inspect
          result<<[cond,pretty(act.action,in_result)]
        end
        result<<pretty(x.default,in_result)
        result.unshift :MultiReduce
      when MultiShift
        h={}
        mods=x.modifiers
        its=[]
        (0...mods.size).step(2){|i| its<<mods[i] }
        x.map.each_with_index{|xx,i| h[i]=pretty(xx) }
        [:MultiShift, its,h]
      when Class; x.name
      when StackMonkey; x.name
      when :accept,:error; x
      else fail "not a valid action: #{x}"
      end
    end

  attr_accessor :inputs

  def all_states
    return @all_states if defined? @all_states
    @all_states=enumerate_states
  end

  def exemplars_that_match p
    @inputs.grep p 
  end

  def pattern_matches_nodes? p
    !@inputs.grep(Node&p).empty?
  end

  def pattern_matches_tokens? p
    !@inputs.grep(Token&p).empty?
  end

  def identity_name_alias? name
    alias_=@identity_name_aliases[name]
    return( alias_||name )
  end

  def compile
    oldparser=Thread.current[:$RedParse_parser]
    Thread.current[:$RedParse_parser]||=self

    if File.exist?("cached_parse_tables.drb")
      dup=Marshal.load(f=open("cached_parse_tables.drb","rb"))
      instance_variables.each{|var| remove_instance_variable var }
      extend SingleForwardable
      def_singleton_delegators(dup,public_methods+private_methods+protected_methods)

      self.inputs=enumerate_exemplars
    else
      @generating_parse_tables=true
      @inputs||=enumerate_exemplars

      states=all_states
#      @rules=expanded_RULES 
      @inputs=nil #Marshal no like it

      begin
        p :dumping
        Marshal.dump(self,f=open("cached_parse_tables.drb","wb"))
        p :dump_done!
      rescue Exception
        p :dump_failed
        File.unlink "cached_parse_tables.drb"
      ensure
        @inputs=enumerate_exemplars
      end
    end
    f.close
   
    #look for unused dotted rules and actions 
    #also states with drs past the end
    past_end=0
    drs=all_dotted_rules
    dr_count=Hash.new(0)
    acts=all_rules#.map{|r| r.action }.uniq
    act_count=Hash.new(0)
    states.each{|state|
      state.dotteds.each{|dr| 
        dr_count[dr]+=1 
        past_end+=1 if dr.pos>=dr.rule.patterns.size
      }
      sav=state.actions.values
      sav.grep(Class|StackMonkey).each{|act| act_count[act.__id__]+=1 }
      sav.grep(MultiReduce|MultiShift).each{|multi| multi.actions.each{|act| act_count[act.__id__]+=1} }
      #p state.name if state.dotteds.select{|dr| dr.rule.action==BeginNode}
    }
    puts "#{past_end} dotted rules found past the end of their rule" if past_end>0
    nevers=0
    drs.each{|dr| 
      next unless dr_count[dr].zero? 
      puts "never reached #{dr.name}" 
      nevers+=1
    }
    puts "#{nevers} dotted rules were never reached (out of #{drs.size})"
    nevers=0
    acts.each{|act|
      next unless act_count[act.__id__].zero?
      puts "never reached #{act.name rescue act}" 
      nevers+=1
    }
    puts  "#{nevers} actions were never reached (out of #{acts.size})"
    p :most_popular_nontrivial_drs
    pp dr_count.reject{|(dr,n)| dr.pos.zero? or dr.pos==1 && dr.rule.lookback?} \
               .sort_by{|(dr,n)| n}[-15..-1].map{|(dr,n)| [dr.name,n] }

    #look for duplicate states
    actions2state={}
    dup_states=0
    states.each{|st| 
      cache=actions2state[st.actions]
      if cache
        st.equivalent_to=cache
        dup_states+=1
      else
        actions2state[st.actions]=st 
      end
    }
    puts "#{dup_states} duplicate states" if dup_states.nonzero?

    name2count={}
    states.each{|state| state.rename(name2count) }

    #divide each state's actions into sr and goto tables
    #also scan states for the most common sr and goto actions and make them default
    states.each{|state| state.make_sr_goto_tables }


#    pp states
#    pp states.size
    
    generate_c $stdout
    return self
  ensure 
    remove_instance_variable :@generating_parse_tables rescue nil
    Thread.current[:$RedParse_parser]=oldparser
  end

  def ultimate_goal_nodes
    result=[]
    all_rules.each{|rule|
      if rule.patterns.size==0 and
         rule.patterns.first==StartToken and
         rule.patterns.last==EoiToken
        result << juice(rule.patterns[1])
      end
    }
    result.flatten!
    return result
  end


#  def start_state
#    goal=ultimate_goal_nodes
#    result=all_rules.select{|rule|
#      rt=rule.reduces_to and
#        !goal.select{|node| node>=rt}.empty?
#    }
#    result.map!{|rule| DottedRule.create(rule,0,parser)}
#
#    result=ParserState.new result
#    result.name="start_state"
#    result
#  end

  def new_state(drs,unruly_also=false)
    result=ParserState.new drs,@states.size
    result.perhaps_also_allow all_rules,self
    cache=@states[result]
    return cache if cache
    @states[result]=@states.size
    return result
  end

  def initial_state
    @states={}
    all_initial_dotted_rules #is this still needed?
    result=new_state all_rules.map{|r| DottedRule.create(r,0,self)}
    result.name="initial"
    #result.perhaps_also_allow all_rules,self #silly here
    result
  end

  attr_reader :states
  
  def start_state
    seenlist = {}
    seenlist.default=:dunno_yet
    result=initial_state.evolve StartToken.new, self,seenlist
    result.perhaps_also_allow all_rules,self
    result.name="start"
    result
    #pp [:initial_seenlist, seenlist]
  #ensure  p :/
  end

  #inline any subsequences in RULES right into the patterns
  #reg should do this already, but current release does not
  def expanded_RULES
    result=RULES()
    return result if (-[:foo, -[:bar]]).subregs.grep(Reg::Subseq).empty?
    result.map!{|rule| 
      unless rule.left.subregs.grep(Reg::Subseq) 
      then rule 
      else
        right=rule.right
        rule=rule.left.subregs.dup
        (rule.size-1).downto(0){|i|
          if Reg::Subseq===rule[i]
            rule[i,1]=rule[i].subregs
          end
        }
        -rule>>right
      end
    }
  end

  module NamedConstant
    attr_accessor :constant_name
    def inspect; constant_name end
  end
  def self.inspect_constant_names
    constants.each{|kn| 
      k=const_get(kn)
      next if Class|Module|Numeric|Symbol|true|false|nil===k
      k.extend NamedConstant
      k.constant_name=kn
    }
  end

  def undumpables
    return @undumpables if @undumpables
    @rules||=expanded_RULES
    n=-1
    @undumpables={}
    abortable_graphwalk(@rules){|cntr,o,i,ty|
      !case o
       when StackMonkey
         @undumpables[o.name]=o
       when Reg::Deferred
         @undumpables[n+=1]=o
         class<<o
           attr_accessor :undump_key
         end
         o.undump_key=n
       end
    }
  end

  class ::Proc #hack hack hack
    #only define hacky _dump if one isn't defined already
    unless instance_methods.include?("_dump") or 
           instance_methods.include?("marshal_dump") or 
           (Marshal.dump(proc{}) rescue false)
      def _dump depth
        undump_key.to_s
      end
      def self._load str
        Thread.current[:$RedParse_parser].undumpables[str.to_i]
      end
    end
  end

=begin disabled, uses too much memory!!
  class MarshalProxy
    def initialize(key)
      @key=key
    end
    attr :key
  end

  #convert unmarshalables, such as stackmonkeys into proxies
  def proxify
    n=-1
    seen={}
    mkproxy=proc{|cntr,o,i,ty,useit|
      case o
      when StackMonkey
        useit[0]=true
        seen[o.__id__]||=MarshalProxy.new(o.name)
      when Reg::Deferred
        useit[0]=true
        seen[o.__id__]||=MarshalProxy.new(n+=1)
      end
    }
    Ron::GraphWalk.graphmodify!(@rules,&mkproxy)
    Ron::GraphWalk.graphmodify!(self,&mkproxy)

  end

  def _dump depth
    fail unless @rules
    proxify
    ivs=instance_variables
    a=ivs+ivs.reverse.map{|var| instance_variable_get var }
    result=Marshal.dump(a,depth)
    unproxify
    return result
  end

  #convert marshal proxies back to the real thing
  def unproxify
    #build a lookup table for unmarshalables by walking @rules
    @rules||=expanded_RULES
    n=-1;lookup={}
    Ron::GraphWalk.graphwalk(@rules){|cntr,o,i,ty|
      case o
      when StackMonkey 
        lookup[o.name]=o
      when Reg::Deferred
        lookup[n+=1]=o 
      end
    }

    Ron::GraphWalk.graphmodify!(self){|cntr,o,i,ty,useit|
      if MarshalProxy===o
        useit[0]=true
        lookup[o.key]
      end
    }
  end

  def self._load(str,*more)
    result=allocate
    a=Marshal.load(str,*more)

    result.unproxify

    (0...a.size/2).each{|i| result.instance_variable_set a[i],a[-i] }
    return result
  end
=end

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
  
    "unary*"=>105, "unary&"=>105, #unary * and & operators
      "lhs*"=>105,  "rhs*"=>105,  #this should remain above =, but other unary stars are below it

    "="=>104,    "%="=>104,   "/="=>104,   "-="=>104,    "+="=>104,
    "|="=>104,   "&="=>104,   ">>="=>104,  "<<="=>104,   "*="=>104,
    "&&="=>104,  "||="=>104,  "**="=>104,  "^="=>104,

    "defined?"=>103,
    "not"=>103,
    ":"=>102, #but not when used as a substitute for 'then'
    "rescue3"=>102,

    "=>"=>101,
    "lhs,"=>100, 
           "rhs,"=>100, #"call,"=>100, "array,"=>100, "param,"=>100,
    ","=>100,
      #the 'precedence' of comma is somewhat controversial. it actually has
      #several different precedences depending on which kind of comma it is.
      #the precedence of , is higher than :, => and the assignment operators 
      #in certain (lhs) contexts. therefore, the precedence of lhs, should 
      #really be above =.

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
  }
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
#    :ident=>/^(?:[+-]@|unary[&*]|(?:lhs|rhs)[*])$/,
    :ident=>/^(?:[+-]@|unary[&])$/,
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
  def lower_op
    return @lower_op if defined? @lower_op
    lower_op=item_that{|op| left_op_higher(@stack[-3],op) }
    lower_op=(LOWEST_OP|(~VALUELIKE_LA & lower_op)).la
    def lower_op.inspect; "lower_op" end
    @lower_op=lower_op
  end

  #this is a hack, should use graphcopy to search for Deferreds and replace with double-Deferred as below
  def item_that(*a,&b)
    if defined? @generating_parse_tables
      huh unless b
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

  BEGINWORDLIST=RubyLexer::BEGINWORDLIST + %w"( [ {"  
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
  FUNCLIKE_KEYWORD=KeywordToken&-{:ident=>RubyLexer::FUNCLIKE_KEYWORDS}
  IGN_SEMI_BEFORE=KW(/^(#{RubyLexer::INNERBOUNDINGWORDS.gsub(/(rescue|then)\|/,'')[1...-1]}|end|[)}\]])$/)|EoiToken
  IGN_SEMI_AFTER=KW(/^(begin|[;:({|]|then|do|else|ensure)$/)|BlockFormalsNode

  #for use in lookback patterns
  OPERATORLIKE_LB=OperatorToken|
                 KW(/^(not | defined\? | rescue3 | .*[@,] | [ ~ ! ; \( \[ \{ ? : ] | \.{1,3} | :: | => | ![=~])$/x)|
                 KW(%r{^( \*\*? | << | >> | &&? | \|\|? | \^ | % | / | - | \+ )?=$}x)|
                 KW(BEGINWORDS)|KW(/^#{INNERBOUNDINGWORDS}$/)|RescueHeaderNode|StartToken|
                 GoalPostToken|BlockFormalsNode

  #for use in lookahead patterns
  VALUELIKE_LA=KW(RubyLexer::VARLIKE_KEYWORDS)|NumberToken|SymbolToken|StringToken|UNOP|DEFOP|
               KW(/^( \( | \{ | )$/x)|VarNameToken|MethNameToken|HerePlaceholderToken|KW(BEGINWORDS)|FUNCLIKE_KEYWORD
  LOWEST_OP=KW(/^(#{ENDWORDS})$/)|KW(/^#{INNERBOUNDINGWORDS.sub('rescue|','')}$/)|EoiToken|GoalPostToken

  RESCUE_BODY=-[Expr.-, RescueNode.*, ElseNode.-, EnsureNode.-,]

  RESCUE_OP=Op('rescue')|(KW('rescue')&-{:infix=>true})

  RESCUE_KW=KW('rescue')&-{:infix=>nil}

  inspect_constant_names

  def RULES
    lower_op= lower_op()

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

    -[Op('=',true)|KW(/^(rescue|when|\[)$/)|Op(/,$/,true),
      Op(/^(?:unary|rhs)\*$/), ValueNode, (MODIFYASSIGNOP|Op('=',true)).la]>>:shift,
    -[MethNameToken|FUNCLIKE_KEYWORD, KW('('), 
      Op(/^(?:unary|rhs)\*$/), ValueNode, (MODIFYASSIGNOP|Op('=',true)).la]>>:shift,
    #star should not be used in an lhs if an rhs or param list context is available to eat it.
    #(including param lists for keywords such as return,break,next,rescue,yield,when)

    #hmmm.... | in char classes below looks useless (predates GoalPostToken)
    -[Op(/^(?:unary|lhs)\*$/), (GoalPostToken|Op(/,$/,true)|KW(/^(in|[=)|;])$/)).la]>>DanglingStarNode, #dangling *
    -[Op(/,$/,true), (GoalPostToken|KW(/^(in|[=)|;])$/)).la]>> #dangling ,
      stack_monkey("DanglingComma",1,DanglingCommaNode){|stack| 
        dcomma=DanglingCommaNode.new
        dcomma.offset=stack.last.offset
        stack.push dcomma, stack.pop
      },

    -[Expr, Op|KW_Op, Expr, lower_op]>>RawOpNode,  #most operators
    
    #assignment
    -[Lvalue, MODIFYASSIGNOP, Expr, lower_op]>>AssignNode,
    -[Lvalue, Op('=',true), AssignmentRhsNode, lower_op]>>AssignNode,
    -[Op('=',true).lb, Expr, lower_op]>>AssignmentRhsNode,

    # a = b rescue c acts like a ternary,,,
    #provided that both a and b are not multiple and b
    #(if it is a parenless callsite) has just 1 param
    -[Lvalue&~MULTIASSIGN, Op('=',true), AssignmentRhsNode&-{:is_list=>true}, 
           Op('rescue3',true), Expr, lower_op]>>AssignNode,
    -[Lvalue&~MULTIASSIGN, Op('=',true), AssignmentRhsNode&-{:is_list=>true}, 
        Op('rescue3',true).la]>>:shift,
    -[Lvalue&~MULTIASSIGN, Op('=',true), AssignmentRhsNode&-{:is_list=>true}, 
        RESCUE_OP.la] >>
        stack_monkey("rescue3",1,Op('rescue3',true)){|stack| 
          resc=stack.last.dup
          resc.ident += '3'
          stack[-1]=resc
        },
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

    -[#(OPERATORLIKE_LB&~(MethNameToken|FUNCLIKE_KEYWORD)).lb, 
      '(', Expr, KW(')')&~(-{:callsite? =>true}|-{:not_real? =>true})]>>ParenedNode,
    -[#(OPERATORLIKE_LB&~(MethNameToken|FUNCLIKE_KEYWORD)).lb, 
      '(', KW(')')&~(-{:callsite? =>true}|-{:not_real? =>true})]>>VarLikeNode, #(), alias for nil

    -[#(OPERATORLIKE_LB&~Op('=',true)).lb, 
      Expr, RESCUE_OP, Expr, lower_op]>>RescueOpNode,

    #dot and double-colon
    -[DoubleColonOp, VarNode,  lower_op]>>ConstantNode,#unary ::
    -[Expr, DotOp, CallNode, lower_op]>>DotCall,      #binary .
    -[Expr, DoubleColonOp, CallNode, lower_op]>>DotCall,    #binary ::
    -[Expr, DoubleColonOp, VarNode, lower_op]>>ConstantNode,#binary ::

    -[Expr, "?", Expr, ":", Expr, lower_op]>>TernaryNode,


    -[MethNameToken, '(', Expr.-, ')', BlockNode.-, KW('do').~.la]>>CallNode,
    -[FUNCLIKE_KEYWORD, '(', Expr.-, ')', BlockNode.-, KW('do').~.la]>>KWCallNode,

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
   -[RubyLexer::VARLIKE_KEYWORDS]>>VarLikeNode,

   #here docs
   -[HerePlaceholderToken]>>HereDocNode,
   -[HereBodyToken.la]>>delete_monkey(1,"delete_here_body"),
   ##this is rediculous. this should be a lexer hack?

   -[VarNameToken]>>VarNode,


  ]
  end

if defined? END_ATTACK
  module Reducer; end
  include Reducer
end

  def initialize(input,name="(eval)",line=1,lvars=[],options={:rubyversion=>1.8})
    @rubyversion=options[:rubyversion]
    if Array===input
      def input.get1token; shift end
      @lexer=input
    else
      @lexer=RubyLexer.new(name,input,line,0,:rubyversion=>@rubyversion)
      lvars.each{|lvar| @lexer.localvars[lvar]=true }
    end
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
  end

  attr_accessor :lexer
  attr :rubyversion

  def get_token(recursing=false)
    unless @moretokens.empty? 
      @last_token=@moretokens.shift
      p @last_token if ENV['PRINT_TOKENS'] unless recursing
      return @last_token
    end

    rpt=ENV['RAW_PRINT_TOKENS']
    begin
      result=@lexer.get1token or break
      p result if rpt

      #set token's line
      result.startline= @endline||=1
      result.endline||=@endline if result.respond_to? :endline=

      if result.respond_to?(:as) and as=result.as
        #result=make_kw(as,result.offset)
        #result.originally=result.ident
        if OperatorToken===result #or KeywordToken===result
          result=result.dup
          result.ident=as
        else
          result=make_kw(as,result.offset)
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

          when /^(#{BINOP_KEYWORDS.join '|'})$/: #should be like this in rubylexer
            result=OperatorToken.new(name,result.offset) unless result.has_end?
          when "|"; result=GoalPostToken.new(result.offset) #is this needed still?
          when "__FILE__"; #I wish rubylexer would handle this
            class<<result; attr_accessor :value; end
            result.value=@file.dup
          when "__LINE__"; #I wish rubylexer would handle this
            class<<result; attr_accessor :value; end
            result.value=@endline
          else 
            result=make_kw name,result.offset if defined? SPECIALIZED_KEYWORDS
            #warning, this may discard information stored in instance vars of result
          end

      when EoiToken; break
      when HereBodyToken; break
      when IgnoreToken; redo
      end
      end
    end while false
    p result if ENV['PRINT_TOKENS'] unless recursing
    return @last_token=result
  end

  def unget_tokens(*tokens)
    @moretokens=tokens.concat @moretokens
  end

  def unget_token(token)
    @moretokens.unshift token
  end

=begin
  self.LOOKAHEAD_CLASSES.each_with_index{|classes,i|
    case classes
    when Class: huh
    when Array: classes.flatten.each{huh}
    else 
    end
  }
=end

#  def fixup_multiple_assignments!; end
end


if __FILE__==$0
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
#    rescue NeverExecThis:
        puts "RedParse attempted to execute parse data in #{name}"
        next
      end
    rescue Interrupt: exit 2
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
      rescue NeverExecThis:
        puts "ParseTree attempted to execute parse data in #{name}"
        next
      rescue Interrupt: exit 2
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

    rescue NeverExecThis: 
      puts "mysterious attempt to execute parse data in #{name}"
      next
    rescue Interrupt,SystemExit: exit 2
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

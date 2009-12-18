=begin
    redparse - a ruby parser written in ruby
    Copyright (C) 2008,2009  Caleb Clausen

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
require "redparse/generate"
require "redparse/cache"

class RedParse
  
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

    def make_sr_goto_tables inputs
      name2exemplar={}
      inputs.each{|i| name2exemplar[i.name]=i }

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
    states.each{|state| state.make_sr_goto_tables @inputs}


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
    unless Proc.new{}.respond_to? :_dump or 
           Proc.new{}.respond_to? :marshal_dump or 
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

end



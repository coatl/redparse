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
#require "redparse/compile"

class RedParse


# irb friendly #inspect/#to_s

  alias :dump :inspect # preserve old inspect functionality
  
  def to_s
    "#<RedParse: [#{@input}]>"
  end
  
  alias :inspect :to_s
  
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

    return @cached_result if defined? @cached_result

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

    @stack.size==2 and return result=NopNode.new #handle empty parse string

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
    return @lower_op if defined? @lower_op
    lower_op=item_that{|op| left_op_higher(@stack[-3],op) }
    lower_op=(LOWEST_OP|(~VALUELIKE_LA & lower_op)).la
    lower_op.extend LowerOp_inspect
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
  FUNCLIKE_KEYWORD=KeywordToken&-{:ident=>RubyLexer::FUNCLIKE_KEYWORDS}
  IGN_SEMI_BEFORE=KW(/^(#{RubyLexer::INNERBOUNDINGWORDS.gsub(/(rescue|then)\|/,'')[1...-1]}|end|[)}\]])$/)|EoiToken
  IGN_SEMI_AFTER=KW(/^(begin|[;:({|]|then|do|else|ensure)$/)|BlockFormalsNode

  #for use in lookback patterns
  OPERATORLIKE_LB=OperatorToken|
                 KW(/^(not | defined\? | rescue3 | .*[@,] | [ ~ ! ; \( \[ \{ ? : ] | \.{1,3} | :: | => | ![=~])$/x)|
                 KW(%r{^( \*\*? | << | >> | &&? | \|\|? | \^ | % | / | - | \+ )?=$}x)|
                 KW(BEGINWORDS)|KW(/^#{INNERBOUNDINGWORDS}$/)|RescueHeaderNode|StartToken|
                 GoalPostToken|BlockFormalsNode|AssignmentRhsListStartToken

  #for use in lookahead patterns
  VALUELIKE_LA=KW(RubyLexer::VARLIKE_KEYWORDS)|NumberToken|SymbolToken|StringToken|UNOP|DEFOP|
               KW(/^[({]$/x)|VarNameToken|MethNameToken|HerePlaceholderToken|
               KW(BEGINWORDS)|FUNCLIKE_KEYWORD|AssignmentRhsListStartToken
  LOWEST_OP=KW(/^(#{ENDWORDS})$/)|KW(/^#{INNERBOUNDINGWORDS.sub('rescue|','')}$/)|
            EoiToken|GoalPostToken|AssignmentRhsListEndToken

  RESCUE_BODY=-[Expr.-, RescueNode.*, ElseNode.-, EnsureNode.-,]

  RESCUE_OP=Op('rescue') #|(KW('rescue')&-{:infix=>true})

  RESCUE_KW=KW('rescue')&-{:infix=>nil}

  inspect_constant_names if respond_to? :inspect_constant_names

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

#    -[Op('=',true)|KW(/^(rescue|when|\[)$/)|Op(/,$/,true),
#      Op(/^(?:unary|rhs)\*$/), ValueNode, (MODIFYASSIGNOP|Op('=',true)).la]>>:shift,
#    -[MethNameToken|FUNCLIKE_KEYWORD, KW('('), 
#      Op(/^(?:unary|rhs)\*$/), ValueNode, (MODIFYASSIGNOP|Op('=',true)).la]>>:shift,
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

  def initialize(input,name="(eval)",line=1,lvars=[],options={})
    @rubyversion=options[:rubyversion]||1.8

    cache=Cache.new(name,line,lvars.sort.join(" "),@rubyversion,self.class.name)
    cache_mode=options[:cache_mode]||:read_write
    raise ArgumentError unless /^(?:read_(?:write|only)|write_only|none)$/===cache_mode.to_s    
    read_cache= /read/===cache_mode.to_s
    input.binmode if input.respond_to? :binmode
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

          when /^(#{BINOP_KEYWORDS.join '|'})$/o #should be like this in rubylexer
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
      when AssignmentRhsListStartToken; break
      when AssignmentRhsListEndToken; break
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

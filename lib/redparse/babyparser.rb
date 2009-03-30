require 'pp'
require 'rubygems'
require 'rubylexer'
require 'reg'

require "babynodes"



class RedParse
  include Nodes

  unless defined? ::Reg::Transform
  #hack, until support for this syntax makes it into the release of reg
    module ::Reg
      class Transform
        def initialize(left,right)
          @left,@right=left,right
        end
        attr_reader :left,:right
      end
      module Reg
        def >>(rep)
          Transform.new(self,rep)
        end
      end
    end    
  end

  #see pickaxe, 1st ed, page 221
  RIGHT_ASSOCIATIVE={
    "**"=>118,   

    "="=>105,    "%="=>105,   "/="=>105,   "-="=>105,    "+="=>105,
    "|="=>105,   "&="=>105,   ">>="=>105,  "<<="=>105,   "*="=>105,
    "&&="=>105,  "||="=>105,  "**="=>105,  "^="=>105,
  }
  PRECEDENCE={
    "::"=>120,    "."=>120,

    "["=>119,    "("=>119,    "{"=>119,     #[] []=

    "**"=>118,   

    "+@"=>117,   "-@"=>117,   "!"=>117,    "~"=>117,

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
  
    "="=>105,    "%="=>105,   "/="=>105,   "-="=>105,    "+="=>105,
    "|="=>105,   "&="=>105,   ">>="=>105,  "<<="=>105,   "*="=>105,
    "&&="=>105,  "||="=>105,  "**="=>105,  "^="=>105,

    "*@"=>104, "&@"=>104, #unary * and & operators

    "defined?"=>103,

    ":"=>102.5,

    "not"=>102,
    "=>"=>101,
    ","=>100,  

    #"unary" prefix function names seen has operators have this precedence
    #but, rubylexer handles precedence of these and outputs fake parens 
    #to tell us how its parsed

    "or"=>99,   "and"=>99,

    "if"=>98,    "unless"=>98,    "while"=>98,    "until"=>98,
 
    ";"=>97,
  }

  Expr=NumberToken|SymbolToken|
       VarNameToken|MethNameToken|
       HerePlaceholderToken|
       ExprNode
    
  UNOP=(OperatorToken|KeywordToken)&-{  #sppflt! KeywordToken here is a hack too
    :ident=>%r[^([*&+-]@|[~!]|not|defined\?)$],
  }

  #these ought to be regular operators, fer gosh sake
  BINOP_KEYWORDS=
    %w[ 
        if unless while until and or rescue 
        && || :: => !~ . .. ...
        : , != == >= <=
        =    %=   /=   -=    \+=
        \|=   &=   >>=  <<=   \*=
        &&=  \|\|=  \*\*=  \^=
    ]
  KeywordOp=
    KeywordToken & -{
      :ident=>/^(#{BINOP_KEYWORDS.join '|'}|[+*?])$/
    }
 
  Op=OperatorToken|KeywordOp

  LowerOp=proc{|list,op2|
      op=list[-2]  
      if Op===op2 or /^[\[({]$/===op2.ident
        rightprec=PRECEDENCE[op2.to_s] or fail "unrecognized right operator: #{op2.inspect}"
        rightprec+=0.001 if RIGHT_ASSOCIATIVE[op2.to_s]
        PRECEDENCE[op.to_s]>=rightprec
      else true
      end
  }

  #rule format: 
  #  syntax pattern_matchers.+, lookahead.-, node type
  RULES=[
    -[UNOP, Expr, LowerOp]>>UnOpNode,
    -[Expr, Op, Expr, LowerOp]>>OpNode,    
    -['(', Expr, ')']>>ParenedNode,    
  ]




  def initialize(input,name="(eval)")
    @lexer=RubyLexer.new(name,input)
  end

  def get_token
    begin 
      case result=@lexer.get1token 
      when OperatorToken:
        /^[*&+-]$/===result.ident and !(Expr===@last) and result.ident<<"@"
      when NewlineToken: 
        result= KeywordToken.new(';',result.offset)
      when EoiToken: 
      when IgnoreToken: 
        redo
      end 
    end while false

    p result if ENV['PRINT_TOKENS']

    return @last=result
  end

  def evaluate rule,stack
    #dissect the rule
    Reg::Transform===rule or fail
    node_type= rule.right
    rule=rule.left.subregs.dup
    lookahead_processor=(rule.pop if Proc===rule.last)
    
    #index of data at which to start matching
    i=stack.size-2   #-1 because last element of stack is always lookahead

    compiled_rule=
      rule.map{|pattern| 
        String|Regexp===pattern ? 
          -{:class=>+KeywordToken, :ident=>pattern} : 
          pattern 
      }

    #what's the minimum stack size this rule could match?
    i>=compiled_rule.size-1 or return false

    matching=[]

    #actually try to match rule elements against each stack element in turn
    compiled_rule.reverse_each{|matcher|
        return false unless matcher===stack[i]  #try match

        matching.unshift stack[i]
        i-=1
    } 


    #give lookahead matcher (if any) a chance to fail the match
    if lookahead_processor
      return false unless lookahead_processor[matching,stack.last] 
    end

    #replace matching elements in stack with node type found
    matchrange= i+1...-1  #what elems in stack were matched?
    if Proc===node_type
      node_type[stack]
    else 
      stack[matchrange]=node_type.new(*matching)
    end
    
    return true #let caller know we found a match

    
  rescue Exception  
    puts "error while executing rule: #{rule.inspect}"
    raise
  end

  def parse
    stack=[get_token] 
           #last token on stack is always implicitly the lookahead
    loop {
      #try all possible reductions
      RULES.reverse_each{|rule|
        evaluate(rule,stack) and break
      } or next 
      
      #no rule can match current stack, get another token
      tok=get_token

      #are we done yet?
      tok.nil? or EoiToken===tok && EoiToken===stack.last and break

      #shift our token onto the stack
      stack.push tok
    }

    #unless the stack is 2 tokens, 
    #with the last an Eoi
    #there was a parse error
    unless stack.size==2
      pp stack[-[15,stack.size].min..-1]
      fail "parse error"
    end
    EoiToken===stack.last or fail

    return stack.first
  end
end


if __FILE__==$0
  output=:pp
  while /^-/===ARGV.first
    case opt=ARGV.shift
      when "--": break
      when "--pp": output=:pp
      when "--lisp": output=:lisp
      when "--parsetree": output=:parsetree
      when "-e": input=ARGV.join(" "); name="-e"; break
      else fail "unknown option: #{opt}"
    end
  end

  
  unless input
    input=open(ARGV.first) rescue STDIN
    name=ARGV.first || "-"
  end
  tree=RedParse.new(input,name).parse
  case output
  when :pp
    pp tree
  when :lisp
    puts tree.to_lisp
  when :parsetree
    pp tree.to_parsetree
  end
end

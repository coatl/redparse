= RedParse
* http://rubyforge.org/projects/redparse
* http://github.com/coatl/redparse

== DESCRIPTION:

RedParse is a ruby parser written in pure ruby. Instead of YACC or 
ANTLR, it's parse tool is a home-brewed "compiler-interpreter". (The
tool is LALR(1)-equivalent and the 'parse language' is pretty nice, 
even in it's current crude form.)

My intent is to have a completely correct parser for ruby, in 100% 
ruby. Currently, RedParse can parse all known ruby 1.8 constructions
correctly. There might be some problems with unparsing or otherwise 
working with texts in a character set other than ascii. Some of the
new ruby 1.9 constructions are supported in 1.9 mode. For more
details on known problems, see below. 

== REQUIREMENTS:
* RedParse requires RubyLexer, my hand-coded lexer for ruby. It also 
  uses Reg, (a pattern-matcher). RubyLexer depends on Sequence, 
  (external iterators). Reg depends on Sequence as well. To summarize:
  *  RedParse 0.8.4 requires RubyLexer 0.7.7 and Reg>=0.4.8
  *  RubyLexer 0.7.7 requires Sequence>=0.2.3
  *  Reg 0.4.8 requires Sequence>=0.2.3
* All are available as gems. (Or tarballs on rubyforge, if you must.)

== INSTALL:
* gem install redparse #(if root as necssary)

== LICENSE:

RedParse is available under the Library General Public License (LGPL).
Please see COPYING.LGPL for details.

== Benefits:

* Pure ruby, through and through. No part is written in C, YACC, 
  ANTLR, lisp, assembly, intercal, befunge or any other language
  except ruby.
* Pretty AST trees (at least, I think so). 
* AST trees closely mirror the actual structure of source code.
* unparser is built in
* ParseTree format output too, if you want that.
* Did I mention that there's no YACC at all? YACC grammars are 
  notoriously difficult to modify, (I've never successfully done it) 
  but I've found it easy, at times even pleasant to modify the parse 
  rules of this grammar as necessary. 
* Relatively small parser: 70 rules in 240 lines 
  (vs (by my count) 320 rules in 2200 lines for MRI 1.8.7. This is 
  by no means a fair comparison, tho, since RubyLexer does a lot 
  more than MRI's lexer, and MRI's 2200 lines include its 
  actions (which occupy somewhere under 3100 lines in RedParse). 
  Also, what is a rule? I counted most things which required a 
  separate action in MRI's parser, I'm not sure if that's fair.
  On the other hand, RedParse rules require no separate actions 
  anywhere.In the end, I still think RedParse is still much easier to 
  understand than MRI's parse.y.) 
* "loosey-goosey" parser happily parses many expressions which normal 
  ruby considers errors.

== Drawbacks:

* Pathetically, ridiculously slow (ok, compiler-compilers are hard...)
* Error handling is very minimal right now.
* No warnings at all.
* Unit test takes a fairly long time.
* Lots of warnings printed during unit test.
* Debugging parse rules is not straightforward.
* Incomplete support for ruby 1.9.
* "loosey-goosey" parser happily parses many expressions which normal 
  ruby considers errors.



== SYNOPSIS:

  #simple example of usage:

  require 'redparse'
  
  parser=RedParse.new("some ruby code here")
  tree=parser.parse

  tree.walk{|parent,i,subi,node|
    case node
    when RedParse::CallNode: #... do something with method calls
    when RedParse::AssignNode: #... maybe alter assignments somehow
    #.... and so on
    end
  }

  #presumably tree was altered somehow in the walk-"loop" above
  #when done mucking with the tree, you can turn it into one
  #of two other formats: ParseTree s-exps or ruby source code.

  tree.to_parsetree #=> turns a tree into an ParseTree-style s-exp.

  tree.unparse({})  #=> turns a tree back into ruby source code.

  #to understand the tree format, you must understand the node classes,
  #which are documented in the next section.

== NODE TYPES:

Syntax trees are represented by trees of nested Nodes. All Nodes descend from 
Array, and their subnodes can be addressed by numeric index, just like normal
Arrays. However, many subnodes want to have names as well, thus most (but not
all) array slots within the various Node classes have names. The general rule
is that Node slots may contain a Node, a plain Array, a String, or nil. 
However, many cases are more specific than that. Specific Node classes are 
documented briefly below in this format:

 NodeName          #comments describing node
                    (slot1: Type, slot2: Type)
                           -OR-
                    (Array[Type*])

Here's an example of how to use this imaginary Node:

 if NodeName===node
   do_something_with node.slot1
   do_something_else_with node.slot2
         #  -OR-
   do_something_with node[0] #slot1
   do_something_else_with node[1] #slot2
 end

Types are specified in an psuedo-BNF syntax. | * + ? all have the same meaning
as in Regexp. Array[Spec] indicates a plain Array (not a Node). The Spec 
describes the constraints on the Array's contents. 
In the cases where node slots don't have names, there will be no colon-
terminated slot name(s) on the second line, just an Array[] specification.

This is a final depiction of the syntax tree. There may be additions to the
existing format in the future, but no incompatibility-creating changes.

 Several abbreviations are used:
 Expr means ValueNode
 LValue means ConstantNode|VarNode|UnaryStarNode|CallNode|
              BracketsGetNode|AssigneeList[LValue*]
 UnAmpNode means UnOpNode with op == "&"

 Node<Array        #abstract ancestor of all nodes
 +-RescueNode      #a rescue clause in a def or begin statement
 |                  (exceptions: Array[Expr*], varname: VarNode|nil, action: Expr)
 +-WhenNode        #a when clause in a case statement
 |                  (when: Expr|Array[Expr+]  then: Expr|nil )
 +-ElsifNode       #an elsif clause in an if statement
 |                  (elsif: Expr, then: Expr|nil) 
 +-ValueNode       #abstract, a node which has a value (an expression)
 |+-VarNode  #represents variables and constants
 ||                 (ident: String)
 |+-ListOpNode     #abstract, ancestor for nodes which are lists of 
 |||               #things separated by some op
 ||+-SequenceNode  #a sequence of statements
 |||                 (Array[Expr*])
 ||+-ConstantNode  #a constant expression of the form A::B::C or the like
 ||                #first expression can be anything
 ||                 (Array[String|Expr|nil,String+])
 |+-RawOpNode      #ancestor of all binary operators (except . :: ; , ?..:)
 |||                (left: Expr, op: String, right: Expr)
 ||+-RangeNode     #a range literal node
 ||+-KeywordOpNode #abstract, ancestor of keyword operators
 |||+-LogicalNode  #and or && || expressions
 |||+-WhileOpNode  #while as an operator
 |||+-UntilOpNode  #until as an operator
 |||+-IfOpNode     #if as an operator
 |||+-UnlessOpNode #unless as an operator
 |||+-RescueOpNode #rescue as an operator
 |||                (body: Expr, rescues: Array[RescueNode*])
 ||+-OpNode        #ancestor of some binary operators (those with methods hidden in them)
 |||+-NotEqualNode #!= expressions
 |||+-MatchNode    #=~ expressions
 |||+-NotMatchNode #!~ expressions
 |+-LiteralNode    #literal symbols, integers
 ||                 (val: Numeric|Symbol|StringNode)
 |+-StringNode     #literal strings
 |||                (Array[(String|Expr)+])
 ||+-HereDocNode   #here documents
 |+-StringCatNode  #adjacent strings are catenated ("foo" "bar" == "foobar")
 ||                 (Array[StringNode+])
 |+-NopNode        #an expression with no tokens at all in it
 ||                 (no attributes)
 |+-VarLikeNode    #nil,false,true,__FILE__,__LINE__,self
 ||                 (name: String)
 |+-UnOpNode       #unary operators
 ||                 (op: String, val: Expr)
 ||+-UnaryStarNode #unary star (splat)
 |||+-DanglingStarNode  #unary star with no argument
 |||||                     (no attributes)
 ||||+-DanglingCommaNode  #comma with no rhs
 ||                        (no attributes)
 |+-BeginNode      #begin..end block
 ||                 (body: Expr|nil, rescues: Array[RescueNode*], 
 ||                  else: Expr|nil, ensure: Expr|nil) 
 |+-ParenedNode    #parenthesized expressions 
 ||                 (body: Expr)
 |+-AssignNode     #assignment (including eg +=)
 ||                 (left:AssigneeList|LValue, op:String ,right:Array[Expr*]|Expr)
 |+-AssigneeList   #abstract, comma-delimited list of assignables
 |||                (Array[LValue*])
 ||+-NestedAssign  #nested lhs, in parentheses
 ||+-MultiAssign   #regular top-level lhs
 ||+-BlockParams   #block formal parameter list
 |+-CallSiteNode   #abstract, method calls
 |||                (receiver: Expr|nil, name: String, params: nil|Array[Expr+,UnaryStarNode?,UnAmpNode?], 
 |||                 blockparams: BlockParams|nil, block: Expr|nil)
 ||+-CallNode      #normal method calls
 ||+-KWCallNode    #keywords that look (more or less) like methods (BEGIN END yield return break continue next)
 |+-ArrayLiteralNode #[..] 
 ||                   (Array[Expr*])
 |+-IfNode         #if..end and unless..end
 ||                 (if: Expr, then: Expr|nil, elsifs: Array[ElsifNode+]|nil, else: Expr|nil)
 |+-LoopNode       #while..end and until..end
 ||                 (while: Expr, do: Expr:nil)
 |+-CaseNode       #case..end
 ||                 (case: Expr|nil, whens: Array[WhenNode*], else: Expr|nil)
 |+-ForNode        #for..end
 ||                 (for: LValue, in: Expr, do: Expr|nil)
 |+-HashLiteralNode #{..}
 ||                  (Array[Expr*]) (size must be even)
 |+-TernaryNode    # ? .. :
 ||                 (if: Expr, then: Expr, else: Expr)
 |+-MethodNode     #def..end
 ||                 (receiver:Expr|nil, name:String, 
 ||                  params:Array[VarNode*,AssignNode*,UnaryStarNode?,UnAmpNode?]|nil, 
 ||                  body: Expr|nil, rescues: Array[RescueNode+]|nil, else: Expr|nil, ensure: Expr|nil)
 |+-AliasNode      #alias foo bar
 ||                 (to: String|VarNode|StringNode, from: String|VarNode|StringNode)
 |+-UndefNode      #undef foo
 ||                 (Array[String|StringNode+])
 |+-NamespaceNode  #abstract
 ||+-ModuleNode    #module..end
 |||                (name: VarNode|ConstantNode, body: Expr|nil
 |||                 rescues: Array[RescueNode+]|nil, else: Expr|nil, ensure: Expr|nil)
 ||+-ClassNode     #class..end
 |||                (name: VarNode|ConstantNode, parent: Expr|nil, body: Expr|nil,
 |||                 rescues: Array[RescueNode+]|nil, else: Expr|nil, ensure: Expr|nil)
 ||+-MetaClassNode #class<<x..end
 ||                 (val: Expr, body: Expr|nil,
 ||                  rescues: Array[RescueNode+]|nil, else: Expr|nil, ensure: Expr|nil)
 |+-BracketsGetNode #a[b]
 |                  (receiver: Expr, params: Array[Expr+,UnaryStarNode?]|nil)
 |
 ErrorNode         #mixed in to nodes with a syntax error
 +-MisparsedNode   #mismatched braces or begin..end or the like

== Known problems with the parser:
* Encoding of the input is not stored anywhere in resulting parse tree.
* Ascii, binary, utf-8, and euc encodings are supported, but sjis is not.

== Known problems with the unparser:
* On unparse, here documents are converted into regular strings. For the most 
  part, these are exactly equivalent to the original. However, whatever tokens
  appeared between the here document header and body will now show up on a 
  different line. If one of those tokens was __LINE__, it will have a 
  different value in the unparsed code than it had originally.
* some floating-point literals don't survive parse/unparse roundtrip intact, 
  due to bugs in MRI 1.8's Float#to_s/String#to_f.
* unparsing of trees whose input was in a character set other than ascii may
  not work.

== Known problems with ParseTree creator
* Major:
  * converting non-ascii encoded parsetrees to ParseTree format doesn't work
* Minor:
  * :begin is not always emitted in the same places as ParseTree does:
    * return begin;  f; end
  * string nodes don't always come out the same way as in ParseTree... 
    but what I emit is equivalent.
    * %W"is #{"Slim #{2?"W":"S"}"}#{xx}."
  * silly empty case nodes aren't always optimized to nop like in ParseTree.

== Bugs in ruby
* These expressions don't parse the same as in MRI because of bug(s) in MRI:
* p = p m %(1)
* p=556;p (e) /a
* c do p (110).m end #buggy in 1.8 only, fixed in 1.9
* module Array ([Array]).first::E; end #buggy in 1.8 only, fixed in 1.9

== Bugs in ParseTree
* Unit tests see failures in these cases, but due to bugs in ParseTree:
* doc_status, err_args = Documeh_status{fcgi_state = 3; docespond do doc_response =fcgi_state =  1; end }
* case F;when G; else;case; when j; end;end
* def foo(a=b=c={}) end
* $11111111111111111111111111111111111111111111111111111111111111111111
* proc{|&b| }
* def sum(options = {:weights => weights = Hash.new(1)}); opt; end
* def foo(a = 1)    end; def foo(a=b=c={})  end; def bar(a=b=c=1,d=2)  end
* yield [a_i, *p] 

= RedParse
* http://rubyforge.org/projects/redparse
* http://github.com/coatl/redparse

== DESCRIPTION:

RedParse is a ruby parser written in pure ruby. Instead of YACC or 
ANTLR, it's parse tool is a home-brewed "compiler-interpreter". (The
tool is LALR(1)-equivalent and the 'parse language' is pretty nice, 
even in it's current crude form.)

My intent is to have a completely correct parser for ruby, in 100% 
ruby. It's not all there yet, but I'm getting pretty close. Currently, 
RedParse can parse slightly in excess of 99% of ruby files found in 
the wild. For known problems, see below. 

== REQUIREMENTS:
* RedParse requires RubyLexer, my hand-coded lexer for ruby. It also 
  uses Reg, (a pattern-matcher). RubyLexer depends on Sequence, 
  (external iterators). Reg depends on Sequence's predecessor, Cursor, 
  altho Cursor isn't used at all in RedParse. The (long-delayed) next 
  version of Reg will use Sequence. To summarize:
  *  RedParse 0.8.2 requires RubyLexer>=0.7.4 and Reg>=0.4.7
  *  RubyLexer 0.7.4 requires Sequence>=0.2.0
  *  Reg 0.4.7 requires Cursor (not really needed here)
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
* Pretty AST trees (at least, I think so). (To program for, not
  necessarily to look at.)
* AST trees closely mirror the actual structure of source code.
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
  But in the end, I still think RedParse is still much easier to 
  understand than MRI's parse.y.) 
* "loosey-goosey" parser happily parses many expressions which normal 
  ruby considers errors.

== Drawbacks:

* Pathetically, ridiculously slow (ok, compiler-compilers are hard...)
* Error handling is very minimal right now.
* No warnings at all.
* Some expressions aren't parsed correctly. see below.
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
 +RescueNode       #a rescue clause in a def or begin statement
 |                  (exceptions: Array[Expr*], varname: VarNode|nil, action: Expr)
 +WhenNode         #a when clause in a case statement
 |                  (when: Expr|Array[Expr+]  then: Expr|nil )
 +ElsifNode        #an elsif clause in an if statement
 |                  (elsif: Expr, then: Expr|nil) 
 +ValueNode        #abstract, a node which has a value (an expression)
 |+VarNode  #represents variables and constants
 ||                 (ident: String)
 |+ListOpNode      #abstract, ancestor for nodes which are lists of 
 |||               #things separated by some op
 ||+SequenceNode   #a sequence of statements
 |||                 (Array[Expr*])
 ||+ConstantNode   #a constant expression of the form A::B::C or the like
 ||                #first expression can be anything
 ||                 (Array[String|Expr|nil,String+])
 |+RawOpNode       #ancestor of all operators (but not . :: ; , ?..:)
 |||                (left: Expr, op: String, right: Expr)
 ||+OpNode         #ancestor of some operators
 |||+RangeNode     #a range literal node
 |||+KeywordOpNode #abstract, ancestor of keyword operators
 ||||+LogicalNode  #and or && || expressions
 ||||+WhileOpNode  #while as an operator
 ||||+UntilOpNode  #until as an operator
 ||||+IfOpNode     #if as an operator
 ||||+UnlessOpNode #unless as an operator
 ||||+RescueOpNode #rescue as an operator
 ||||               (body: Expr, rescues: Array[RescueNode*])
 |||+NotEqualNode  #!= expressions
 |||+MatchNode     #=~ expressions
 |||+NotMatchNode  #!~ expressions
 |+LiteralNode     #literal symbols, integers
 ||                 (val: Numeric|Symbol|StringNode)
 |+StringNode      #literal strings
 |||                (Array[(String|Expr)+])
 ||+HereDocNode    #here documents
 |+StringCatNode   #adjacent strings are catenated ("foo" "bar" == "foobar")
 ||                 (Array[StringNode+])
 |+NopNode         #an expression with no tokens at all in it
 ||                 (no attributes)
 |+VarLikeNode     #nil,false,true,__FILE__,__LINE__,self
 ||                 (name: String)
 |+UnOpNode        #unary operators
 ||                 (op: String, val: Expr)
 ||+UnaryStarNode  #unary star (splat)
 |||+DanglingStarNode  #unary star with no argument
 |||||                    (no attributes)
 ||||+DanglingCommaNode  #comma with no rhs
 ||                       (no attributes)
 |+BeginNode       #begin..end block
 ||                 (body: Expr|nil, rescues: Array[RescueNode*], 
 ||                  else: Expr|nil, ensure: Expr|nil) 
 |+ParenedNode     #parenthesized expressions 
 ||                 (body: Expr)
 |+AssignNode      #assignment (including eg +=)
 ||                 (left:AssigneeList|LValue, op:String ,right:Array[Expr*]|Expr)
 |+AssigneeList    #abstract, comma-delimited list of assignables
 |||                (Array[LValue*])
 ||+NestedAssign   #nested lhs, in parentheses
 ||+MultiAssign    #regular top-level lhs
 ||+BlockParams    #block formal parameter list
 |+CallSiteNode    #abstract, method calls
 |||                (receiver: Expr|nil, name: String, params: nil|Array[Expr+,UnaryStarNode?,UnAmpNode?], 
 |||                 block_params: BlockParams, block: Expr)
 ||+CallNode       #normal method calls
 ||+KWCallNode     #keywords that look (more or less) like methods (BEGIN END yield return break continue next)
 |+ArrayLiteralNode #[..] 
 ||                 (Array[Expr*])
 |+IfNode          #if..end and unless..end
 ||                 (if: Expr, then: Expr|nil, elsifs: Array[ElsifNode+]|Nil, else: Expr|nil)
 |+LoopNode        #while..end and until..end
 ||                 (while: Expr, do: Expr:nil)
 |+CaseNode        #case..end
 ||                 (case: Expr|nil, whens: Array[WhenNode*], else: Expr|nil)
 |+ForNode         #for..end
 ||                 (for: LValue, in: Expr, do: Expr|nil)
 |+HashLiteralNode #{..}
 ||                 (Array[Expr*]) (size must be even)
 |+TernaryNode     # ? .. :
 ||                 (if: Expr, then: Expr, else: Expr)
 |+MethodNode      #def..end
 ||                 (receiver:Expr|nil, name:String, 
 ||                  params:Array[VarNode*,AssignNode*,UnaryStarNode?,UnAmpNode?]|nil, 
 ||                  body: Expr|nil, rescues: Array[RescueNode+]|nil, else: Expr|nil, ensure: Expr|nil)
 |+AliasNode       #alias foo bar
 ||                 (to: String|VarNode|StringNode, from: String|VarNode|StringNode)
 |+UndefNode       #undef foo
 ||                 (Array[String|StringNode+])
 |+NamespaceNode #abstract
 ||+ModuleNode     #module..end
 |||                (name: VarNode|ConstantNode, body: Expr|nil
 |||                 rescues: Array[RescueNode+]|nil, else: Expr|nil, ensure: Expr|nil)
 ||+ClassNode      #class..end
 |||                (name: VarNode|ConstantNode, parent: Expr|nil, body: Expr|nil,
 |||                 rescues: Array[RescueNode+]|nil, else: Expr|nil, ensure: Expr|nil)
 ||+MetaClassNode  #class<<x..end
 ||                 (val: Expr, body: Expr|nil,
 ||                  rescues: Array[RescueNode+]|nil, else: Expr|nil, ensure: Expr|nil)
 |+BracketsGetNode #a[b]
 |                  (receiver: Expr, params: Array[Expr+,UnaryStarNode?]|nil)
 |
 ErrorNode         #mixed in to nodes with a syntax error
 +MisparsedNode    #mismatched braces or begin..end or the like



== Known failing expressions
* The following expressions are known to parse incorrectly currently:
* def foo(a = 1)    end; def foo(a=b=c={})  end; def bar(a=b=c=1,d=2)  end
* <<-EOS<<__LINE__
    EOS
* z = valueo_s rescue "?"
* self.<=>:p8
* return (@images = @old_imgs)
* p ?e.<<?y
* doc_status, err_args = Documeh_status{fcgi_state = 3; docespond do doc_response =fcgi_state =  1; end }
* class A;def b;class <<self;@@p = false end;end;end
* return @senders[1] =
    2

== Not exactly right, but semantically equivalent
* These don't return exactly the same s-exp as MRI/ParseTree, but close enough:  
* for i in (begin
    [44,55,66,77,88] end) do p i**Math.sqrt(i) end
* %W"is #{"Slim #{2?"W":"S"}"}#{xx}."
* def d; return (block_given? ? begin; yield f; ensure; f.close; end : f); end
* "#{publi}#{}>"
* /__A#{__FILE__}tcase/n =~ i

== Bugs in ruby
* These expressions don't parse the same as in MRI because of bug(s) in MRI:
* p = p m %(1)
* p=556;p (e) /a
* c do p (110).m end

== Bugs in ParseTree
* Unit tests see failures in these cases, but due to bugs in ParseTree:
* case F;when G; else;case; when j; end;end
* def foo(a=b=c={}) end
* $11111111111111111111111111111111111111111111111111111111111111111111
* proc{|&b| }
* def sum(options = {:weights => weights = Hash.new(1)}); opt; end

==Known failing files
*  And here's a list of files which are known to parse incorrectly:


activerdf-1.6.10/lib/active_rdf/queryengine/query2jars2.rb
aquarium-0.4.2/spec/aquarium/aspects/aspect_spec.rb
aspectr-0-3-5/lib/aspectr.rb
authorails-1.0.0/lib/initializer.rb
bibliomori-0.2.3/src/filter.rb
cursor-0.9/cursor/circular.rb
cursor-0.9/duck.rb
data_objects-0.9.2/lib/data_objects/transaction.rb
docdiff-0.3.2/docdiff/charstring.rb
extract_curves-0.0.1/ruby_libs/pav/string/observable.rb
extract-curves-0.1.1/ruby_libs/pav/string/observable.rb
gecoder-with-gecode-0.8.2/lib/gecoder/interface/constraints/set/connection.rb
gettext-1.91.0/test/test_gettext.rb
lazytools-0.1.0/lib/dirstructure.rb
lockfile-1.4.3/lib/lockfile-1.4.3.rb
lockfile-1.4.3/lib/lockfile.rb
logging-0.8.0/test/test_logger.rb
logmerge-1.0.0/lib/logmerge/resolv.rb
main-2.8.0/lib/main/base.rb
muding-0.2.0/lib/initializer.rb
net-mdns-0.4/lib/net/dns/resolv.rb
not_naughty-0.5.1/spec/not_naughty_spec.rb
radiant-0.6.7/vendor/rails/railties/lib/initializer.rb
redparse/test/data/def_spec.rb
reg/regcompiler.rb
rgen-0.4.2/redist/xmlscan/tests/deftestcase.rb
rq-3.4.0/lib/rq/lockfile.rb
ruby-ajp-0.2.1/lib/net/ajp13.rb
ruby-contract-0.1.1/lib/contract/overrides.rb
ruby-debug-0.10.0/cli/ruby-debug/processor.rb
rubygame-2.3.0/lib/rubygame/rect.rb
rubygems-update-1.2.0/lib/rubygems/test_utilities.rb
rubylexer/test/data/heremonsters_dos.rb
ruby-nuggets-0.2.1.246/lib/nuggets/array/to_hash.rb
sequel_core-2.2.0/lib/sequel_core/core_sql.rb
skktools/filters/skkdictools.rb
stick-1.3.3/lib/stick/matrix.rb
stomp-1.0.5/lib/stomp.rb
syntax-1.0.0/lib/syntax/lang/ruby.rb
tournament-1.1.0/lib/tournament/bracket.rb
typo-5.0.3.98.1/config/environment.rb
typo-5.0.3.98.1/vendor/syntax/lib/syntax/lang/ruby.rb
/usr/lib/ruby/1.8/docdiff/charstring.rb
/usr/lib/ruby/1.8/puppet/provider/nameservice.rb
/usr/lib/ruby/1.8/qwik/util-css.rb
/usr/lib/ruby/1.8/resolv.rb
/usr/lib/ruby/1.8/rexml/doctype.rb
/usr/lib/ruby/1.8/tkextlib/bwidget/buttonbox.rb
/usr/lib/ruby/1.9.0/resolv.rb
/usr/lib/ruby/1.9.0/rexml/doctype.rb
/usr/share/doc/libtcltk-ruby1.8/examples/tkextlib/tkHTML/ss.rb
/usr/share/doc/libtcltk-ruby1.9/examples/tkextlib/tkHTML/ss.rb
/usr/share/doc/libxml-ruby/tests/tc_xml_parser8.rb
/usr/share/glark/options.rb
/usr/share/hiki/hiki/docdiff/charstring.rb
/usr/share/nadoka/rice/irc.rb
/usr/share/rails/actionpack/test/controller/caching_test.rb
/usr/share/rails/actionpack/test/controller/fragment_store_setting_test.rb
/usr/share/rails/activesupport/lib/active_support/multibyte/handlers/utf8_handler.rb
/usr/share/rails/railties/builtin/rails_info/rails/info.rb
/usr/share/rails/railties/lib/initializer.rb
/usr/share/skktools/filters/skkdictools.rb
/usr/share/tdiary/TC_tdiary-setup.rb
wwwsrv-0.15.3/wwwsrv/fastcgi.rb
xmlscan-0.2.3/tests/deftestcase.rb
ya2yaml-0.26/lib/ya2yaml.rb
ActiveRecord-JDBC-0.5/lib/jdbc_adapter/jdbc_hsqldb.rb
ruby-web-1.1.1/lib/webunit/parser.rb
filemanager-0.2.0/filemanager/app/controllers/fm/filemanager_controller.rb

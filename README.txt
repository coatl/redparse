= RedParse
* redparse.rubyforge.org
* rubyforge.org/projects/redparse

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
  *  RedParse 0.9.0 requires RubyLexer>=0.7.1 and Reg>=0.4.7
  *  RubyLexer 0.7.1 requires Sequence>=0.2.0
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

* Pathetically, rediculously slow (to be addressed soon).
* Error handling is very minimal right now.
* No warnings at all.
* Some expressions aren't parsed correctly. see below.
* Line numbers in ParseTrees not supported yet. 
* AST tree format is not finalized yet.
* Unit test takes a fairly long time.
* Lots of warnings printed during unit test.
* Debugging parse rules is not straightforward.
* No support for ruby 1.9.
* No support for any charset but ascii (until rubylexer gets it).
* "loosey-goosey" parser happily parses many expressions which normal 
  ruby considers errors.

== SYNOPSIS:

  #simple example of usage:

  require 'redparse'
  
  parser=RedParse.new("some ruby code here")
  tree=parser.parse

  tree.walk{|parent,i,subi,node|
    case node
    when CallNode: #... do something with method calls
    when AssignNode: #... maybe alter assignments somehow
    #.... and so on
    end
  }

  #presumably tree was altered somehow in the walk-"loop" above
  #when done mucking with the tree, you can turn it into one
  #of two other formats: ParseTree s-exps or (experimental)
  #ruby source code.

  tree.to_parsetree #=> turns a tree into an ParseTree-style s-exp.

  tree.unparse({})  #=> turns a tree back into ruby source code.

  #to understand the tree format, you must understand the node classes,
  #which are documented in the next section.

== NODE TYPES:

Syntax trees are represented by trees of nested Nodes. All Nodes descend from 
Array, and their subnodes can be addressed by numeric index, just like normal
Arrays. However, many subnodes want to have names as well, thus most (but not
all) array slots within the various Node classes have names. The general rule
is that Node slots may contain a Node, a VarNameToken, a plain Array, a 
String, or nil. However, many cases are more specific than that. Specific 
Node classes are documented briefly below in this format:

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

And just to make everyone's life a little more complicated, I should add that
there is still one token type which leaks through into the syntax tree:
VarNameToken. It's descended from a RubyLexer::Token, not a Node. In the 
future this will be replaced with an actual Node.

In fact, there are several areas where rough edges in the syntax tree could be
removed. So don't consider this current depiction to be final. But it should 
be pretty close.

 Several abbreviations are used:
 Value means ValueNode
 LValue means ConstantNode|VarNameToken|UnaryStarNode|CallNode|
              BracketsGetNode|AssigneeList[LValue*]
 UnAmpNode means UnOpNode with op == "&"

 VarNameToken<RubyLexer::Token  #represents variables and constants
                                 (ident: String)
 Node<Array        #abstract ancestor of all nodes (except VarNameToken)
 +RescueNode       #a rescue clause in a def of begin statement
 |                  (exceptions: Array[Value*], varname: VarNameToken|nil, action: Value)
 +WhenNode         #a when clause in a case statement
 |                  (when: Value|Array[Value+]  then: Value|nil )
 +ElsifNode        #an elsif clause in an if statement
 |                  (elsif: Value, then: Value|nil) 
 +ValueNode        #abstract, a node which has a value (an expression)
 |+ListOpNode      #abstract, ancestor for nodes which are lists of 
 |||               #things separated by some op
 ||+SequenceNode   #a sequence of statements
 |||                 (Array[Value*])
 ||+ConstantNode   #a constant expression of the form A::B::C or the like
 ||                #first expression can be anything
 ||                 (Array[String|Value|nil,String+])
 |+RawOpNode       #ancestor of all operators (but not . :: ; , ?..:)
 |||                (left: Value, op: String, right: Value)
 ||+OpNode         #ancestor of some operators
 |||+RangeNode     #a range literal node
 |||+KeywordOpNode #abstract, ancestor of keyword operators
 ||||+LogicalNode  #and or && || expressions
 ||||+WhileOpNode  #while as an operator
 ||||+UntilOpNode  #until as an operator
 ||||+IfOpNode     #if as an operator
 ||||+UnlessOpNode #unless as an operator
 |||+NotEqualNode  #!= expressions
 |||+MatchNode     #=~ expressions
 |||+NotMatchNode  #!~ expressions
 |+LiteralNode     #literal symbols, integers
 ||                 (val: Numeric|Symbol|StringNode)
 |+StringNode      #literal strings
 |||                (Array[(String|Value)+])
 ||+HereDocNode    #here documents
 |+StringCatNode   #adjacent strings are catenated ("foo" "bar" == "foobar")
 ||                 (Array[StringNode+])
 |+NopNode         #an expression with no tokens at all in it
 ||                 (no attributes)
 |+VarLikeNode     #nil,false,true,__FILE__,__LINE__,self
 ||                 (name: String)
 |+UnOpNode        #unary operators
 ||                 (op: String, val: Value)
 ||+UnaryStarNode  #unary star (splat)
 |||+DanglingStarNode  #unary star with no argument
 |||||                    (no attributes)
 ||||+DanglingCommaNode  #comma with no rhs
 ||                       (no attributes)
 |+ParenedNode     #ugly, parenthesized expressions and begin..end
 ||                 (body: Value)  -OR-      (parentheses)
 ||                 (body: Value|nil, rescues: Array[RescueNode*], 
 ||                  else: Value|nil, ensure: Value|nil)  (begin...end and rescue as operator)
 |+AssignNode      #assignment (including eg +=)
 ||                 (left:AssigneeList|LValue, op:String ,right:Array[Value*]|Value)
 |+AssigneeList    #abstract, comma-delimited list of assignables
 |||                (Array[LValue*])
 ||+NestedAssign   #nested lhs, in parentheses
 ||+MultiAssign    #regular top-level lhs
 ||+BlockParams    #block formal parameter list
 |+CallSiteNode    #abstract, method calls
 |||                (receiver: Value|nil, name: String, params: nil|Array[Value+,UnaryStarNode?,UnAmpNode?], 
 |||                 block_params: BlockParams, block: Value)
 ||+CallNode       #normal method calls
 ||+KWCallNode     #keywords that look (more or less) like methods (BEGIN END yield return break continue next)
 |+ArrayLiteralNode #[..] 
 ||                 (Array[Value*])
 |+IfNode          #if..end and unless..end
 ||                 (if: Value, then: Value|nil, elsifs: Array[ElsifNode+]|Nil, else: Value|nil)
 |+LoopNode        #while..end and until..end
 ||                 (while: Value, do: Value:nil)
 |+CaseNode        #case..end
 ||                 (case: Value|nil, whens: Array[WhenNode*], else: Value|nil)
 |+ForNode         #for..end
 ||                 (for: LValue, in: Value, do: Value|nil)
 |+HashLiteralNode #{..}
 ||                 (Array[Value*]) (size must be even)
 |+TernaryNode     # ? .. :
 ||                 (if: Value, then: Value, else: Value)
 |+MethodNode      #def..end
 ||                 (receiver:Value|nil, name:String, 
 ||                  params:Array[VarNameToken*,AssignNode*,UnaryStarNode?,UnAmpNode?]|nil, 
 ||                  body: Value|nil, rescues: Array[RescueNode+]|nil, else: Value|nil, ensure: Value|nil)
 |+AliasNode       #alias foo bar
 ||                 (to: String|VarNameToken|StringNode, from: String|VarNameToken|StringNode)
 |+UndefNode       #undef foo
 ||                 (Array[String|StringNode+])
 |+NamespaceNode #abstract
 ||+ModuleNode     #module..end
 |||                (name: VarNameToken|ConstantNode, body: Value|nil)
 ||+ClassNode      #class..end
 |||                (name: VarNameToken|ConstantNode, parent: Value|nil, body: Value|nil)
 ||+MetaClassNode  #class<<x..end
 ||                 (val: Value, body: Value|nil)
 |+BracketsGetNode #a[b]
 |                  (receiver: Value, params: Array[Value+,UnaryStarNode?]|nil)
 |
 ErrorNode         #mixed in to nodes with a syntax error
 +MisparsedNode    #mismatched braces or begin..end or the like



== Known failing expressions
*  The following expressions are known to parse incorrectly currently:
* m.cn= 1, V
* "#{}"""
* $11111111111111111111111111111111111111111111111111111111111111111111
* begin;mode;rescue;o_chmod rescue nil;end
* case F;when G; else;case; when j; end;end
* def i;"..#{@@c = 1}";end
* e { |c|; print "%02X" % c }
* File.open() {|f|  ;  }
* %W(white\  \  \ \  \ space).should == ["white ", " ", "  ", " space"]
* %w[- \\ e]
* %w[- \\ ]
* module A; b; rescue C=>d; e; else g; ensure f; end
* class A; b; rescue C=>d; e; else g; ensure f; end
* class<<A; b; rescue C=>d; e; else g; ensure f; end

== Homie doan' play dat
* These expressions don't parse the same as in MRI, I believe because of 
  bug(s) in MRI. 
* p = p m %(1)
* p=556;p (e) /a

==Known failing files
*  And here's a list of files which are known to parse incorrectly:
* alib-0.5.1/lib/alib-0.5.1/util.rb
* stick-1.3.3/test/test_matrix.rb
* rubyslippers-1.03/extras/rconftool.rb
* rubysync-0.2.1/lib/ruby_sync/connectors/base_connector.rb
* karmasphere-client-0.6.4/lib/karmasphere/query.rb
* roby-0.7.2/test/test_transactions.rb
* rwddemo-0.92/extras/rconftool.rb
* rmail-1.0.0/test/testmessage.rb
* passenger-2.0.1/lib/passenger/abstract_server.rb
* rubywebdialogs/rubywebdialogs.rb
* aspectr-0.3.7/lib/aspectr.rb
* rubywebdialogs-0.2.0/realstuff.rb
* stick-1.3.3/lib/stick/matrix.rb
* rb2html-2.4/rb2html/haskell_lexer.rb
* mspire-0.3.9/lib/spec_id/srf.rb
* rb-wartslib-0.9.14/lib/wartslib/wl-file.rb
* sqliki_generator-0.0.4/templates/lib_sanitize_html.rb
* rwdshell-1.00/extras/rconftool.rb
* main-2.8.0/lib/main/base.rb
* pseudoxml-0.1.0/RAKEFILE
* rubytorrent-0.3/lib/rubytorrent/server.rb
* Nephila-0.6.0/config/environment.rb
* ruby-rpm-1.2.3/lib/rpm.rb
* ya2yaml-0.26/lib/ya2yaml.rb
* spree-0.2.0/vendor/rails/activesupport/lib/active_support/multibyte/handlers/utf8_handler.rb
* rq-3.4.0/rails/vendor/rails/actionpack/test/controller/fragment_store_setting_test.rb
* typo-5.0.3.98.1/config/environment.rb
* aeditor-1.9/lib/aeditor/test_lexer.rb
* gecoder-with-gecode-0.8.2/lib/gecoder/interface/constraints/set/connection.rb
* onebody-0.3.0/vendor/rails/actionpack/test/controller/caching_test.rb
* rwdhypernote-0.10/extras/rconftool.rb
* radiant-0.6.7/vendor/rails/railties/lib/initializer.rb
* rwdgutenberg-0.07/extras/rconftool.rb
* aspectr-0-3-5/lib/aspectr.rb
* rubygems-update-1.2.0/lib/rubygems/test_utilities.rb
* live_console-0.1.0/lib/live_console.rb
* html-table-1.3.1/test/tc_header.rb
* authorails-1.0.0/lib/initializer.rb
* allinoneruby-0.2.11/realstuff.rb
* typo-5.0.3.98.1/db/migrate/040_attach_content_to_blog.rb
* rq-3.4.0/lib/rq/lockfile.rb
* rubygame-2.3.0/lib/rubygame/rect.rb
* html-table-1.3.1/test/tc_foot.rb
* html-table-1.3.1/test/tc_caption.rb
* extract-curves-0.1.1/ruby_libs/pav/string/observable.rb
* starling-0.9.3/lib/starling/runner.rb
* RbYAML-0.2.0/lib/rbyaml/types.rb
* rwdaddresses-1.03/extras/vpim/rrule.rb
* Dnsruby-1.0/test/tc_tsig.rb
* rubyscript2exe/rubyscript2exe.rb
* rq-3.4.0/rails/vendor/rails/railties/builtin/rails_info/rails/info.rb
* extract_curves-0.0.1/ruby_libs/pav/string/observable.rb
* simplemapper-0.0.6/lib/simple_mapper/default_plugins/oauth.rb
* text-hyphen-1.0.0/lib/text/hyphen/language/id.rb
* ruby-nxt-0.8.1/examples/commands.rb
* gettext-1.91.0/test/test_rails_caching.rb
* rwdschedule-1.02/extras/rconftool.rb
* rabbit-0.5.6/lib/rabbit/image/base.rb
* sqliki-0.0.4/templates/lib_sanitize_html.rb
* whistle-0.1.1/lib/resource.rb
* html-table-1.3.1/examples/intermediate2.rb
* xmlparser-0.6.8/xmlparser/samples/digesttest.rb
* datamapper-0.3.2/lib/data_mapper/adapters/sql/mappings/table.rb
* openwferu-0.9.17/test/raw_prog_test.rb
* rubytorrent-0.3/lib/rubytorrent/controller.rb
* rails-2.1.0/builtin/rails_info/rails/info.rb
* mongrel-1.1.5/lib/mongrel.rb
* stream-0.5/lib/stream.rb
* dhaka-2.2.1/lib/dhaka/lexer/alphabet.rb
* roby-0.7.2/lib/roby/planning/model.rb
* data_objects-0.9.2/lib/data_objects/connection.rb
* logging-0.8.0/test/test_logger.rb
* hobo-0.7.5/hobo_files/plugin/lib/hobo/scopes/defined_scope_proxy_extender.rb
* logtwuncator-0.1.2/lib/log_twuncator/win32_file.rb
* Dnsruby-1.0/test/tc_axfr.rb
* spree-0.2.0/vendor/rails/railties/builtin/rails_info/rails/info.rb
* margot-0.5.0/Rakefile
* ruby-style-1.2.1/lib/style.rb
* preferences-0.1.3/test/functional/preferences_test.rb
* mogilefs-client-1.2.1/lib/mogilefs/backend.rb
* flatulent-0.0.4/lib/flatulent.rb
* actionpack-2.1.0/test/controller/caching_test.rb
* buildr-1.3.1.1/lib/buildr/packaging/zip.rb
* typo-5.0.3.98.1/vendor/syntax/lib/syntax/lang/ruby.rb
* starling-0.9.3/lib/starling/server.rb
* ruby-odbc-0.9995/ruby-odbc-0.9995/extconf.rb
* ruby-ajp-0.2.1/lib/net/ajp13.rb
* radiant-0.6.7/vendor/rails/railties/builtin/rails_info/rails/info.rb
* xmlparser-0.6.8/xmlparser/samples/digesttest2.rb
* rwdtorrent-0.05/extras/rubytorrent/controller.rb
* typo-5.0.3.98.1/db/migrate/039_serialize_blog_attributes.rb
* sparrow-0.3.1/lib/sparrow/runner.rb
* rubyscript2exe-0.5.3/realstuff.rb
* stomp-1.0.5/lib/stomp.rb
* adhearsion-0.7.7/lib/rami.rb
* bigtinker-0.93/extras/rconftool.rb
* Buildr-0.17.0/lib/tasks/zip.rb
* rmail-1.0.0/test/testheader.rb
* datamapper-0.3.2/lib/data_mapper/adapters/sql/mappings/column.rb
* fhlow-1.91.0/lib/module_cmdparse/cmdparse.rb
* can_has_bots-0.1.0/adapters/jabber/bot.rb
* spree-0.2.0/vendor/rails/actionpack/test/controller/caching_test.rb
* nitro-0.41.0/lib/nitro/compiler/include.rb
* wwwsrv-0.15.3/test/test_token.rb
* wwwsrv-0.15.3/wwwsrv/fastcgi.rb
* rgen-0.4.2/redist/xmlscan/tests/deftestcase.rb
* muding-0.2.0/lib/initializer.rb
* bj-1.0.1/lib/bj/runner.rb
* dnsruby-1.1/test/tc_tsig.rb
* ruote-0.9.18/test/ft_0c_testname.rb
* aquarium-0.4.2/spec/aquarium/aspects/aspect_spec.rb
* Ruby-MemCache-0.0.1/tests/stats.tests.rb
* docdiff-0.3.2/docdiff/charstring.rb
* automateit-0.80624/lib/automateit/account_manager/posix.rb
* simplemapper-0.0.6/lib/simple_mapper/default_plugins/associations.rb
* onebody-0.3.0/vendor/rails/activesupport/lib/active_support/multibyte/handlers/utf8_handler.rb
* notification-0.2/Rakefile
* websitary-0.5/lib/websitary/htmldiff.rb
* feedupdater-0.2.5/lib/feed_updater/vendor/daemons/daemonize.rb
* rogdl-0.2.3/lib/string.rb
* yapra-0.1.1/legacy_plugins/Filter/sort.rb
* rwdaddresses-1.03/extras/rconftool.rb
* ruote-0.9.18/test/raw_prog_test.rb
* sequel_core-2.2.0/lib/sequel_core/core_sql.rb
* html-table-1.3.1/examples/advanced.rb
* swivel-0.0.175/vendor/activesupport-2.0.2-/lib/active_support/multibyte/handlers/utf8_handler.rb
* Rubernate-0.1.7/tests/rubernate/impl/dbi_generic_stub.rb
* logmerge-1.0.0/lib/logmerge/resolv.rb
* mongoose-0.2.5/lib/mongoose/column.rb
* aquarium-0.4.2/spec/aquarium/aspects/advice_chain_node_spec.rb
* qualitysmith_extensions-0.0.64/lib/qualitysmith_extensions/object/ancestry_of_method.rb
* rubygame-2.3.0/samples/chimp.rb
* rshell-0.1.0/lib/g/misc.rb
* bibliomori-0.2.3/src/filter.rb
* Ruby-MemCache-0.0.4/lib/memcache.rb
* raggle-0.4.2/extras/test_html_renderer.rb
* vpim-0.619/lib/vpim/rrule.rb
* command-set-0.10.4/lib/command-set/arguments.rb
* activerdf-1.6.10/lib/active_rdf/queryengine/query2jars2.rb
* hobofields-0.7.5/lib/hobo_fields/field_spec.rb
* mssqlclient-0.1.0/RAKEFILE
* html-table-1.3.1/test/tc_head.rb
* ruby-odbc-0.9995/ruby-odbc-0.9995/utf8/extconf.rb
* ditz-0.3/lib/operator.rb
* tolerances-1.0.0/lib/tolerance.rb
* raingrams-0.0.9/lib/raingrams/multigram_model.rb
* cheat-1.2.1/lib/site.rb
* ap4r-0.3.6/lib/ap4r/mongrel.rb
* radiant-0.6.7/vendor/rails/actionpack/test/controller/fragment_store_setting_test.rb
* datamapper-0.3.2/lib/data_mapper/associations/belongs_to_association.rb
* rools-0.4/RAKEFILE
* gettext-1.91.0/test/test_gettext.rb
* multibyte-0.0.1/lib/multibyte/handlers/utf8_handler.rb
* data_objects-0.9.2/lib/data_objects/transaction.rb
* onebody-0.3.0/vendor/rails/railties/builtin/rails_info/rails/info.rb
* mongrel_esi-0.5.4/test/net/server_test.rb
* dbc-2.0.0/lib/dbc/ocl.rb
* rubylexer/test/data/heremonsters_dos.rb
* lib/ftools.rb
* lib/rdoc/generators/template/html/html.rb
* lib/yaml/types.rb
* ruby-1.8.7/sample/test.rb
* ruby-1.8.7/test/fileutils/test_fileutils.rb
* ruby-1.8.7/test/ruby/test_proc.rb
* reg/regcompiler.rb
* reg/regvar.rb
* rubyparser/rubyparser.rb
* /usr/lib/ruby/1.9.0/resolv.rb
* /usr/lib/ruby/1.8/cmdparse2.rb
* /usr/lib/ruby/1.8/puppet/provider/package/appdmg.rb
* /usr/lib/ruby/1.8/qwik/util-css.rb
* /usr/lib/ruby/1.8/rbbr/doc/ri2.rb
* libxml-parser-ruby1.8/examples/digesttest.rb
* glark/optproc.rb
* hiki/hiki/plugin.rb
* kagemai/lib/xmlscan/parser.rb
* actionpack/test/controller/caching_test.rb
* activesupport/lib/active_support/multibyte/handlers/utf8_handler.rb
* railties/builtin/rails_info/rails/info.rb
* railties/lib/initializer.rb
* skktools/filters/skkdictools.rb
* cursor-0.9/duck.rb
* Ron-0.1.0/lib/ron.rb
* ruby-debug-0.10.0/cli/ruby-debug/processor.rb

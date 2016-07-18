#encoding: binary
$pt_known_output={
  "" => [],
  "  " => [],
  "   " => [],
  "     " => [],
  "\
        assert_equal 403291461126605635584000000, 26._!
        assert_equal 1, 0._!
        assert_equal 1, 1._!
        assert_equal 24, 4._!\

" =>
    [[:block, [:fcall, :assert_equal, [:array, [
    :lit, 403291461126605635584000000], [:call, [:lit, 26], :_!]
    ]], [:fcall, :assert_equal, [:array, [:lit, 1], [:call, [
    :lit, 0], :_!]]], [:fcall, :assert_equal, [:array, [:lit, 1], [
    :call, [:lit, 1], :_!]]], [:fcall, :assert_equal, [:array, [
    :lit, 24], [:call, [:lit, 4], :_!]]]]],

  "\
        location_display = if(location.size == 1)
          location[0].sub(/\\A(.+:\\d+).*/, ' [\\1]')
        else
          \"\\n\#{location.join(\"\\n\")}\"
        end
        \"Failure:\\n\#@test_name\#{location_display}:\\n\#@message\"" =>
    [[:block, [:lasgn, :location_display, [:if, [:call, [:call, [
    :vcall, :location], :size], :==, [:array, [:lit, 1]]], [
    :call, [:call, [:vcall, :location], :[], [:array, [:lit, 0]]
    ], :sub, [:array, [:lit, /\A(.+:\d+).*/], [:str, " [\\1]"]]
    ], [:dstr, "\n", [:evstr, [:call, [:vcall, :location], :join, [
    :array, [:str, "\n"]]]]]]], [:dstr, "Failure:\n", [:evstr, [
    :ivar, :@test_name]], [:evstr, [:lvar, :location_display]], [
    :str, ":\n"], [:evstr, [:ivar, :@message]]]]],

  "\
        raise AttributeConstraintError( #and a comment here
        )" =>
    [[:fcall, :raise, [:array, [:fcall, :AttributeConstraintError]
    ]]],

  "\
        raise AttributeConstraintError( #and a comment here
     #and here
        )" =>
    [[:fcall, :raise, [:array, [:fcall, :AttributeConstraintError]
    ]]],

  "\
        raise AttributeConstraintError( #and a comment here
     #and here
=begin look!
and here too
=end look!
        )" =>
    [[:fcall, :raise, [:array, [:fcall, :AttributeConstraintError]
    ]]],

  "\
        raise AttributeConstraintError( #and a comment here
     #and here
=begin look!
and here too
=end
        )" =>
    [[:fcall, :raise, [:array, [:fcall, :AttributeConstraintError]
    ]]],

  "\
        raise AttributeConstraintError( #and a comment here
     #and here
=begin
=end
        )" =>
    [[:fcall, :raise, [:array, [:fcall, :AttributeConstraintError]
    ]]],

  "\
        raise AttributeConstraintError( #and a comment here
     #and here
=begin
and here too
=end look!
        )" =>
    [[:fcall, :raise, [:array, [:fcall, :AttributeConstraintError]
    ]]],

  "\
        raise AttributeConstraintError( #and a comment here
     #and here
=begin
and here too
=end
        )" =>
    [[:fcall, :raise, [:array, [:fcall, :AttributeConstraintError]
    ]]],

  "\
        raise AttributeConstraintError(
        )" =>
    [[:fcall, :raise, [:array, [:fcall, :AttributeConstraintError]
    ]]],

  "\
      <<-\"foo\".count('\\\\')==0
      \"
      foo" =>
    [[:call, [:call, [:str, "      \"\n"], :count, [:array, [
    :str, "\\"]]], :==, [:array, [:lit, 0]]]],

  "\
      <<-\"foo\".count('\\\\')==0
      \\\"
      foo" =>
    [[:call, [:call, [:str, "      \"\n"], :count, [:array, [
    :str, "\\"]]], :==, [:array, [:lit, 0]]]],

  "\
      <<-\"foo\".count('\\\\')==1
      \\\\\"
      foo" =>
    [[:call, [:call, [:str, "      \\\"\n"], :count, [:array, [
    :str, "\\"]]], :==, [:array, [:lit, 1]]]],

  "\
      <<-\"foo\".count('\\\\')==1
      \\\\\\\"
      foo" =>
    [[:call, [:call, [:str, "      \\\"\n"], :count, [:array, [
    :str, "\\"]]], :==, [:array, [:lit, 1]]]],

  "\
      <<-\"foo\".count('\\\\')==2
      \\\\\\\\\"
      foo" =>
    [[:call, [:call, [:str, "      \\\\\"\n"], :count, [:array, [
    :str, "\\"]]], :==, [:array, [:lit, 2]]]],

  "\
      <<-\"foo\".count('\\\\')==2
      \\\\\\\\\\\"
      foo" =>
    [[:call, [:call, [:str, "      \\\\\"\n"], :count, [:array, [
    :str, "\\"]]], :==, [:array, [:lit, 2]]]],

  "\
      <<-'foo'.count('\\\\')==2
      \\\\'
      foo" =>
    [[:call, [:call, [:str, "      \\\\'\n"], :count, [:array, [
    :str, "\\"]]], :==, [:array, [:lit, 2]]]],

  "\
      <<-'foo'.count('\\\\')==3
      \\\\\\'
      foo" =>
    [[:call, [:call, [:str, "      \\\\\\'\n"], :count, [:array,
     [:str, "\\"]]], :==, [:array, [:lit, 3]]]],

  "\
      <<-'foo'.count('\\\\')==4
      \\\\\\\\'
      foo" =>
    [[:call, [:call, [:str, "      \\\\\\\\'\n"], :count, [:array, [
    :str, "\\"]]], :==, [:array, [:lit, 4]]]],

  "\
      <<-'foo'.count('\\\\')==5
      \\\\\\\\\\'
      foo" =>
    [[:call, [:call, [:str, "      \\\\\\\\\\'\n"], :count, [
    :array, [:str, "\\"]]], :==, [:array, [:lit, 5]]]],

  "\
      begin
        lcode = IO::readlines( @xGAsaFile.untaint ).untaint
        code = lcode.join( \" \" )
        @oAspHandler.load_code( code )
      rescue => e
        if Asp::Common::debug == 1 
          result = @oApacheRequest.escape_html(e.message)
          @oApacheRequest.server.log_error( \"[Ruby/ASP] [%d] : (%s #%d) : %s\", $$, __FILE__, __LINE__, result )
        end
        return false
      end" =>
    [[:rescue, [:block, [:lasgn, :lcode, [:call, [:call, [:const,
     :IO], :readlines, [:array, [:call, [:ivar, :@xGAsaFile],
     :untaint]]], :untaint]], [:lasgn, :code, [:call, [
    :lvar, :lcode], :join, [:array, [:str, " "]]]], [:call, [
    :ivar, :@oAspHandler], :load_code, [:array, [:lvar, :code]]]
    ], [:resbody, nil, [:block, [:lasgn, :e, [:gvar, :$!]], [
    :if, [:call, [:call, [:colon2, [:const, :Asp], :Common], :debug], :==, [
    :array, [:lit, 1]]], [:block, [:lasgn, :result, [:call, [
    :ivar, :@oApacheRequest], :escape_html, [:array, [:call, [
    :lvar, :e], :message]]]], [:call, [:call, [
    :ivar, :@oApacheRequest], :server], :log_error, [:array, [
    :str, "[Ruby/ASP] [%d] : (%s #%d) : %s"], [:gvar, :$$], [
    :str, "(string)"], [:lit, 8], [:lvar, :result]]]], nil], [
    :return, [:false]]]]]],

  "      define_method :foo &proc{:bar}" =>
    [[:fcall, :define_method, [:array, [:call, [:lit, :foo], :&,
     [:array, [:iter, [:fcall, :proc], nil, [:lit, :bar]]]]]]],

  "      define_method :foo, &proc{:bar}" =>
    [[:block_pass, [:iter, [:fcall, :proc], nil, [:lit, :bar]], [
    :fcall, :define_method, [:array, [:lit, :foo]]]]],

  "      define_method(:foo) &proc{:bar}" =>
    [[:call, [:fcall, :define_method, [:array, [:lit, :foo]]], :&, [
    :array, [:iter, [:fcall, :proc], nil, [:lit, :bar]]]]],

  "      define_method(:foo, &proc{:bar})" =>
    [[:block_pass, [:iter, [:fcall, :proc], nil, [:lit, :bar]], [
    :fcall, :define_method, [:array, [:lit, :foo]]]]],

  "      p  *nil" =>
    [[:fcall, :p, [:splat, [:nil]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "      p  *nil, &nil" =>
    [[:block_pass, [:nil], [:fcall, :p, [:splat, [:nil]]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "      p :k, *nil" =>
    [[:fcall, :p, [:argscat, [:array, [:lit, :k]], [:nil]]]],

  "      p :k, *nil, &nil" =>
    [[:block_pass, [:nil], [:fcall, :p, [:argscat, [:array, [
    :lit, :k]], [:nil]]]]],

  "\
     %w[ ac
         df]" =>
    [[:array, [:str, "ac"], [:str, "df"]]],

  "\
     <<-\"foo\"
       \"
     foo" =>
    [[:str, "       \"\n"]],

  "\
     <<-\"foo\"
       \\\"
     foo" =>
    [[:str, "       \"\n"]],

  "\
     <<-\"foo\"
       \\\\\"
     foo" =>
    [[:str, "       \\\"\n"]],

  "\
     <<-\"heredoc\"
       a b c \#{d}
     heredoc" =>
    [[:dstr, "       a b c ", [:evstr, [:vcall, :d]], [:str, "\n"]
    ]],

  "\
     <<-\"heredoc\"
       a b c 
     heredoc" =>
    [[:str, "       a b c \n"]],

  "\
     <<-\"heredoz\"
       a b c \#{d}
     heredoz" =>
    [[:dstr, "       a b c ", [:evstr, [:vcall, :d]], [:str, "\n"]
    ]],

  "\
     <<-'foo'.count('\\\\')==0
       '
     foo" =>
    [[:call, [:call, [:str, "       '\n"], :count, [:array, [
    :str, "\\"]]], :==, [:array, [:lit, 0]]]],

  "\
     <<-'foo'.count('\\\\')==1
       \\'
     foo" =>
    [[:call, [:call, [:str, "       \\'\n"], :count, [:array, [
    :str, "\\"]]], :==, [:array, [:lit, 1]]]],

  "\
     <<-'heredoc'
       a b c \#{d}
     heredoc" =>
    [[:str, "       a b c \#{d}\n"]],

  "\
     <<-'heredoc'
       a b c 
     heredoc" =>
    [[:str, "       a b c \n"]],

  "\
     <<-`foo`
       \\\\`
     foo" =>
    [[:xstr, "       \\`\n"]],

  "\
     <<-`foo`
       \\`
     foo" =>
    [[:xstr, "       `\n"]],

  "\
     <<-`heredoc`
       a b c \#{d}
     heredoc" =>
    [[:dxstr, "       a b c ", [:evstr, [:vcall, :d]], [:str, "\n"]
    ]],

  "\
     <<-`heredoc`
       a b c 
     heredoc" =>
    [[:xstr, "       a b c \n"]],

  "\
     <<-heredoc
       a b c 
     heredoc" =>
    [[:str, "       a b c \n"]],

  "\
     <<-heredoc
     heredoc" =>
    [[:str, ""]],

  "\
     <<`heredoc`
       a b c \#{d}
heredoc" =>
    [[:dxstr, "       a b c ", [:evstr, [:vcall, :d]], [:str, "\n"]
    ]],

  "\
     <<`heredoc`
       a b c 
heredoc" =>
    [[:xstr, "       a b c \n"]],

  "\
     <<heredoc
       a b c 
heredoc" =>
    [[:str, "       a b c \n"]],

  "\
     a b, c
=begin x
y
=end z
__END__
     d e, f" =>
    [[:fcall, :a, [:array, [:vcall, :b], [:vcall, :c]]]],

  "\
     a b, c
__END__
     d e, f" =>
    [[:fcall, :a, [:array, [:vcall, :b], [:vcall, :c]]]],

  "\
     begin 
       a
     else
       f
     end" =>
    [[:block, [:vcall, :a], [:vcall, :f]],
    {:warnings=>["(string):5: warning: else without rescue is useless"]}],

  "\
     begin 
       a
     else
       f
     ensure
       e
     end" =>
    [[:ensure, [:block, [:vcall, :a], [:vcall, :f]], [:vcall, :e]
    ],
    {:warnings=>["(string):7: warning: else without rescue is useless"]}],

  "\
     begin 
       a
     rescue B=>c
       d
     else
       f
     end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:const, :B]], [
    :block, [:lasgn, :c, [:gvar, :$!]], [:vcall, :d]]], [:vcall,
     :f]]],

  "\
     begin 
       a
     rescue B=>c
       d
     else
       f
     ensure
       e
     end" =>
    [[:ensure, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :const, :B]], [:block, [:lasgn, :c, [:gvar, :$!]], [:vcall,
     :d]]], [:vcall, :f]], [:vcall, :e]]],

  "\
     begin 
       a
     rescue B=>c
       d
     end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:const, :B]], [
    :block, [:lasgn, :c, [:gvar, :$!]], [:vcall, :d]]]]],

  "\
     begin 
       a
     rescue B=>c
       d
     ensure
       e
     end" =>
    [[:ensure, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :const, :B]], [:block, [:lasgn, :c, [:gvar, :$!]], [:vcall,
     :d]]]], [:vcall, :e]]],

  "\
     begin 
       a
     rescue B=>c
     end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:const, :B]], [
    :lasgn, :c, [:gvar, :$!]]]]],

  "\
     begin 
       a
     rescue B
       c
     end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:const, :B]], [
    :vcall, :c]]]],

  "\
     begin 
       a
     rescue
       b
     end" =>
    [[:rescue, [:vcall, :a], [:resbody, nil, [:vcall, :b]]]],

  "\
     formatter.format_element(element) do
       amrita_expand_and_format1(element, context, formatter)
     end\

" =>
    [[:iter, [:call, [:vcall, :formatter], :format_element, [
    :array, [:vcall, :element]]], nil, [:fcall, :amrita_expand_and_format1, [
    :array, [:vcall, :element], [:vcall, :context], [
    :vcall, :formatter]]]]],

  "\
     p <<-heredoc \"x y z\" and 5
       a b c
     heredoc" =>
    [[:and, [:fcall, :p, [:array, [:str, "       a b c\nx y z"]]
    ], [:lit, 5]]],

  "\
     p <<-heredoc + \"dfgsf\"
       a b c 
     heredoc" =>
    [[:fcall, :p, [:array, [:call, [:str, "       a b c \n"], :+, [
    :array, [:str, "dfgsf"]]]]]],

  "\
     p <<-heredoc + \"sdfsdF\" and 5
       a b c 
     heredoc" =>
    [[:and, [:fcall, :p, [:array, [:call, [:str, "       a b c \n"], :+, [
    :array, [:str, "sdfsdF"]]]]], [:lit, 5]]],

  "\
     p <<-heredoc
       a b c 
     heredoc" =>
    [[:fcall, :p, [:array, [:str, "       a b c \n"]]]],

  "\
     p(<<-heredoc)
       a b c 
     heredoc" =>
    [[:fcall, :p, [:array, [:str, "       a b c \n"]]]],

  "     return gsub() {}" =>
    [[:return, [:iter, [:fcall, :gsub], nil]]],

  "\
    /:(\\d+)/ =~ caller[0]
    file = $`
    line = $1.to_i
    code = <<\"End\"
    define_method(\"test_id2ref_\#{line}\") {\\
      o = ObjectSpace._id2ref(obj.object_id);\\
      assert_same(obj, o, \"didn't round trip: \\\#{obj.inspect}\");\\
    }
End" =>
    [[:block, [:match2, [:lit, /:(\d+)/], [:call, [:vcall, :caller], :[], [
    :array, [:lit, 0]]]], [:lasgn, :file, [:back_ref, :`]], [
    :lasgn, :line, [:call, [:nth_ref, 1], :to_i]], [:lasgn, :code, [
    :dstr, "    define_method(\"test_id2ref_", [:evstr, [:lvar,
     :line]], [
    :str, "\") {      o = ObjectSpace._id2ref(obj.object_id);      assert_same(obj, o, \"didn't round trip: \#{obj.inspect}\");    }\n"]
    ]]]],

  "\
    <<-foo+\"str\"
    foo" =>
    [[:call, [:str, ""], :+, [:array, [:str, "str"]]]],

  "\
    assert(begin
         for k,v in y
           raise if k*2 != v
         end
         true
       rescue
         false
       end)" =>
    [[:fcall, :assert, [:array, [:rescue, [:block, [:for, [
    :vcall, :y], [:masgn, [:array, [:lasgn, :k], [:lasgn, :v]],
     nil, nil], [:if, [:call, [:call, [:lvar, :k], :*, [:array, [
    :lit, 2]]], :==, [:array, [:lvar, :v]]], nil, [:vcall, :raise]
    ]], [:true]], [:resbody, nil, [:false]]]]]],

  "\
    a{<<-f;b}+
     \#{b=1}
    f
    c" =>
    [[:call, [:iter, [:fcall, :a], nil, [:block, [:dstr, "     ", [
    :evstr, [:dasgn_curr, :b, [:lit, 1]]], [:str, "\n"]], [:dvar,
     :b]]], :+, [:array, [:vcall, :c]]],
    {:warnings=>["(string):2: warning: useless use of a literal in void context"]}],

  "\
    a{<<-f;b}
     \#{b=1}
    f" =>
    [[:iter, [:fcall, :a], nil, [:block, [:dstr, "     ", [:evstr, [
    :dasgn_curr, :b, [:lit, 1]]], [:str, "\n"]], [:dvar, :b]]],
    {:warnings=>["(string):2: warning: useless use of a literal in void context"]}],

  "\
    a{<<-f;b}\\
     \#{b=1}
    f
    +c" =>
    [[:call, [:iter, [:fcall, :a], nil, [:block, [:dstr, "     ", [
    :evstr, [:dasgn_curr, :b, [:lit, 1]]], [:str, "\n"]], [:dvar,
     :b]]], :+, [:array, [:vcall, :c]]],
    {:warnings=>["(string):2: warning: useless use of a literal in void context"]}],

  "\
    begin
          xOutCode = @oXmlScript.script( xCode ) 
    rescue XmlScriptError => e
          @error_line = e.line
          @error_message = e.message
    end" =>
    [[:rescue, [:lasgn, :xOutCode, [:call, [:ivar, :@oXmlScript],
     :script, [:array, [:vcall, :xCode]]]], [:resbody, [:array, [
    :const, :XmlScriptError]], [:block, [:lasgn, :e, [:gvar, :$!]
    ], [:iasgn, :@error_line, [:call, [:lvar, :e], :line]], [
    :iasgn, :@error_message, [:call, [:lvar, :e], :message]]]]]],

  "\
    case
    when 0
      guecoding
    else case
      when eucjp_match_length
        guing
      end
    end" =>
    [[:case, nil, [:when, [:array, [:lit, 0]], [:vcall, :guecoding]
    ], [:when, [:array, [:vcall, :eucjp_match_length]], [:vcall,
     :guing]]]],

  "\
    case
    when true
      assert(true)
    when false, nil
      assert(false)
    else
      assert(false)
    end" =>
    [[:case, nil, [:when, [:array, [:true]], [:fcall, :assert, [
    :array, [:true]]]], [:when, [:array, [:false], [:nil]], [
    :fcall, :assert, [:array, [:false]]]], [:fcall, :assert, [
    :array, [:false]]]]],

  "\
    def _make_regex(str) /([\#{Regexp.escape(str)}])/n end
    p _make_regex(\"8smdf,34rh\\\#@\\\#$%$gfm/[]dD\")\

" =>
    [[:block, [:defn, :_make_regex, [:scope, [:block, [:args, :str], [
    :dregx, "([", [:evstr, [:call, [:const, :Regexp], :escape, [
    :array, [:lvar, :str]]]], [:str, "])"], 16]]]], [:fcall, :p,
     [:array, [:fcall, :_make_regex, [:array, [
    :str, "8smdf,34rh\#@\#$%$gfm/[]dD"]]]]]]],

  "\
    def x# [ruby-dev:24228]
    assert_nothing_raised {
      def temporally_method_for_test_eval_and_define_method(&block)
        lambda {
          class << Object.new; self end.__send__(:define_method, :zzz, &block)
        }
      end
      v = eval(\"temporally_method_for_test_eval_and_define_method {}\")
      {}[0] = {}
      v.call
    }
    end" =>
    [[:defn, :x, [:scope, [:block, [:args], [:iter, [
    :fcall, :assert_nothing_raised], nil, [:block, [
    :defn, :temporally_method_for_test_eval_and_define_method, [
    :scope, [:block, [:args], [:block_arg, :block], [:iter, [
    :fcall, :lambda], nil, [:block_pass, [:lvar, :block], [:call, [
    :sclass, [:call, [:const, :Object], :new], [:scope, [:self]]
    ], :__send__, [:array, [:lit, :define_method], [:lit, :zzz]]
    ]]]]]], [:dasgn_curr, :v, [:fcall, :eval, [:array, [
    :str, "temporally_method_for_test_eval_and_define_method {}"]
    ]]], [:attrasgn, [:hash], :[]=, [:array, [:lit, 0], [:hash]]
    ], [:call, [:dvar, :v], :call]]]]]]],

  "\
    events=[]
    set_trace_func(Proc.new { |event, file, lineno, mid, bidning, klass|
      events << [event, lineno, mid, klass]
    })
    a = 1
    foo
    a
    b = 1 + 2
    if b == 3
      case b
      when 2
        c = \"b == 2\"
      when 3
        c = \"b == 3\"
      end
    end" =>
    [[:block, [:lasgn, :events, [:zarray]], [:fcall, :set_trace_func, [
    :array, [:iter, [:call, [:const, :Proc], :new], [:masgn, [
    :array, [:dasgn_curr, :event], [:dasgn_curr, :file], [
    :dasgn_curr, :lineno], [:dasgn_curr, :mid], [:dasgn_curr,
     :bidning], [:dasgn_curr, :klass]], nil, nil], [:call, [
    :lvar, :events], :<<, [:array, [:array, [:dvar, :event], [
    :dvar, :lineno], [:dvar, :mid], [:dvar, :klass]]]]]]], [
    :lasgn, :a, [:lit, 1]], [:vcall, :foo], [:lvar, :a], [:lasgn,
     :b, [:call, [:lit, 1], :+, [:array, [:lit, 2]]]], [:if, [
    :call, [:lvar, :b], :==, [:array, [:lit, 3]]], [:case, [
    :lvar, :b], [:when, [:array, [:lit, 2]], [:lasgn, :c, [:str,
     "b == 2"]]], [:when, [:array, [:lit, 3]], [:lasgn, :c, [
    :str, "b == 3"]]], nil], nil]],
    {:warnings=>["(string):7: warning: useless use of a variable in void context"]}],

  "\
    funcnames.collect{|fn| <<-endeval; hn }.to_s(fn,hn,gn=1)
      \#{fn} \#{gn} \#{hn=2}
    endeval
    p fn,gn,hn" =>
    [[:block, [:call, [:iter, [:call, [:vcall, :funcnames], :collect], [
    :dasgn_curr, :fn], [:block, [:dstr, "      ", [:evstr, [
    :dvar, :fn]], [:str, " "], [:evstr, [:vcall, :gn]], [:str, " "], [
    :evstr, [:dasgn_curr, :hn, [:lit, 2]]], [:str, "\n"]], [
    :dvar, :hn]]], :to_s, [:array, [:vcall, :fn], [:vcall, :hn],
     [:lasgn, :gn, [:lit, 1]]]], [:fcall, :p, [:array, [:vcall,
     :fn], [:lvar, :gn], [:vcall, :hn]]]],
    {:warnings=>["(string):2: warning: useless use of a literal in void context"]}],

  "\
    return @senders[1] =
      2" =>
    [[:return, [:attrasgn, [:ivar, :@senders], :[]=, [:array, [
    :lit, 1], [:lit, 2]]]]],

  "\
   %r{^(
                class|module|end|self|true|false|nil|def|  
                __FILE__|__LINE__|(\\})\\)
              )$}x" =>
    [[
    :lit, /^(
                class|module|end|self|true|false|nil|def|  
                __FILE__|__LINE__|(\})\)
              )$/x]
    ],

  "\
   /sdf
    sdf
          sdfsdf
     sdfsf/" =>
    [[:lit, /sdf
    sdf
          sdfsdf
     sdfsf/]],

  "\
   /sdf
    sdf\\
          sdfsdf
     sdfsf/" =>
    [[:lit, /sdf
    sdf          sdfsdf
     sdfsf/]],

  "\
   modelCode = <<ENDModelCode
   
class \#{@modelClassName} < ActiveRecord::Base
  def self.listAll
      find_by_sql(\"{'0'.CT.''}\")
  end
end
\nENDModelCode
\n" =>
    [[:lasgn, :modelCode, [:dstr, "   \nclass ", [:evstr, [:ivar,
     :@modelClassName]], [
    :str, " < ActiveRecord::Base\n  def self.listAll\n      find_by_sql(\"{'0'.CT.''}\")\n  end\nend\n\n"]]]],

  "\
  \"\#{p \"\#{<<-kekerz}\#{\"foob\"
     zimpler
     kekerz
     }\"
  }\"" =>
    [[:dstr, "", [:evstr, [:fcall, :p, [:array, [
    :str, "     zimpler\nfoob"]]]]]],

  "\
  %Q[\\
]" =>
    [[:str, ""]],

  "\
  %W[
]" =>
    [[:zarray]],

  "\
  %W[\\
]" =>
    [[:array, [:str, "\n"]]],

  "\
  %W[a b \\
]" =>
    [[:array, [:str, "a"], [:str, "b"], [:str, "\n"]]],

  "\
  %W[a b\\
]" =>
    [[:array, [:str, "a"], [:str, "b\n"]]],

  "\
  %W[a\\
]" =>
    [[:array, [:str, "a\n"]]],

  "\
  %[\\
]" =>
    [[:str, ""]],

  "\
  %q[\\
]" =>
    [[:str, "\\\n"]],

  "\
  %r[\\
]" =>
    [[:lit, //]],

  "\
  %s[\\
]" =>
    [[:lit, :"\\\n"]],

  "\
  %w[
]" =>
    [[:zarray]],

  "\
  %w[\\
]" =>
    [[:array, [:str, "\n"]]],

  "\
  %x[\\
]" =>
    [[:xstr, ""]],

  "\
  /\\
/" =>
    [[:lit, //]],

  "  1.break {}.size" =>
    [[:call, [:iter, [:call, [:lit, 1], :break], nil], :size]],

  "  1.next {}.size" =>
    [[:call, [:iter, [:call, [:lit, 1], :next], nil], :size]],

  "  1.raise {}.to_s+\"****\"" =>
    [[:call, [:call, [:iter, [:call, [:lit, 1], :raise], nil],
     :to_s], :+, [:array, [:str, "****"]]]],

  "  1.return {}.size" =>
    [[:call, [:iter, [:call, [:lit, 1], :return], nil], :size]],

  "  1.throw {}.to_s+\"****\"" =>
    [[:call, [:call, [:iter, [:call, [:lit, 1], :throw], nil],
     :to_s], :+, [:array, [:str, "****"]]]],

  "\
  <<-foo+'123
  abc
  foo
  456'" =>
    [[:call, [:str, "  abc\n"], :+, [:array, [:str, "123\n  456"]
    ]]],

  "\
  <<-foo+<<-bar
    a b c
  foo
    d e f
  bar" =>
    [[:call, [:str, "    a b c\n"], :+, [:array, [:str, "    d e f\n"]
    ]]],

  "\
  @@anon = Module.new
  class @@anon::I
    bindtextd 
    def test2    
      _()      
    end
  end
  module @@anon::J
    bindt
  end" =>
    [[:block, [:cvdecl, :@@anon, [:call, [:const, :Module], :new]
    ], [:class, [:colon2, [:cvar, :@@anon], :I], nil, [:scope, [
    :block, [:vcall, :bindtextd], [:defn, :test2, [:scope, [
    :block, [:args], [:fcall, :_]]]]]]], [:module, [:colon2, [
    :cvar, :@@anon], :J], [:scope, [:vcall, :bindt]]]]],

  "\
  WIDGET_DESTROY_HOOK = '<WIDGET_DESTROY_HOOK>'
  INTERP._invoke_without_enc('event', 'add', 
                             \"<\#{WIDGET_DESTROY_HOOK}>\", '<Destroy>')
  INTERP._invoke_without_enc('bind', 'all', \"<\#{WIDGET_DESTROY_HOOK}>\",
                             install_cmd(proc{|path|
                                unless TkCore::INTERP.deleted?
                                  begin
                                    if (widget=TkCore::INTERP.tk_windows[path])
                                      if widget.respond_to?(:__destroy_hook__)
                                        widget.__destroy_hook__
                                      end
                                    end
                                  rescue Exception=>e
                                      p e if $DEBUG
                                  end
                                end
                             }) << ' %W')
\n
\n" =>
    [[:block, [:cdecl, :WIDGET_DESTROY_HOOK, [:str, "<WIDGET_DESTROY_HOOK>"]], [
    :call, [:const, :INTERP], :_invoke_without_enc, [:array, [
    :str, "event"], [:str, "add"], [:dstr, "<", [:evstr, [:const,
     :WIDGET_DESTROY_HOOK]], [:str, ">"]], [:str, "<Destroy>"]]
    ], [:call, [:const, :INTERP], :_invoke_without_enc, [:array,
     [:str, "bind"], [:str, "all"], [:dstr, "<", [:evstr, [
    :const, :WIDGET_DESTROY_HOOK]], [:str, ">"]], [:call, [
    :fcall, :install_cmd, [:array, [:iter, [:fcall, :proc], [
    :dasgn_curr, :path], [:if, [:call, [:colon2, [:const, :TkCore],
     :INTERP], :deleted?], nil, [:rescue, [:if, [:dasgn_curr, :widget, [
    :call, [:call, [:colon2, [:const, :TkCore], :INTERP], :tk_windows], :[], [
    :array, [:dvar, :path]]]], [:if, [:call, [:dvar, :widget],
     :respond_to?, [:array, [:lit, :__destroy_hook__]]], [:call,
     [:dvar, :widget], :__destroy_hook__], nil], nil], [:resbody, [
    :array, [:const, :Exception]], [:block, [:dasgn_curr, :e, [
    :gvar, :$!]], [:if, [:gvar, :$DEBUG], [:fcall, :p, [:array, [
    :dvar, :e]]], nil]]]]]]]], :<<, [:array, [:str, " %W"]]]]]]],

  "  alias :\"==\" :e " => [[:alias, [:lit, :==], [:lit, :e]]],
  "  alias :\"==\\1\" :e " =>
    [[:alias, [:lit, :"==\x01"], [:lit, :e]]],

  "  alias :'=$=' :e " => [[:alias, [:lit, :"=$="], [:lit, :e]]],
  "  alias :'==' :e " => [[:alias, [:lit, :==], [:lit, :e]]],
  "  alias <=> p" => [[:alias, [:lit, :<=>], [:lit, :p]]],
  "  alias P? p" => [[:alias, [:lit, :P?], [:lit, :p]]],
  "  alias [] p" => [[:alias, [:lit, :[]], [:lit, :p]]],
  "  alias p? p" => [[:alias, [:lit, :p?], [:lit, :p]]],
  "  break {}.size" => [[:break, [:call, [:hash], :size]]],
  "\
  case  
  a;
  when  
  b;
  then  
  c;
  end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b]], [
    :vcall, :c]], nil]],

  "\
  case 
  a
  when 
  b
  then 
  c
  end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b]], [
    :vcall, :c]], nil]],

  "\
  case a
  when b
  then c
  end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b]], [
    :vcall, :c]], nil]],

  "  def a(b) {} end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b], [:hash]]]]],

  "  def a.b i; end" =>
    [[:defs, [:vcall, :a], :b, [:scope, [:args, :i]]]],

  "  def a.b(c) {} end" =>
    [[:defs, [:vcall, :a], :b, [:scope, [:block, [:args, :c], [
    :hash]]]]],

  "  def b i; end  " =>
    [[:defn, :b, [:scope, [:block, [:args, :i], [:nil]]]]],

  "\
  def evaluate rule,stack
    #dissect the rule
  rescue Exception  
    puts \"error while executing rule: \#{}\"
  end" =>
    [[:defn, :evaluate, [:scope, [:block, [:args, :rule, :stack], [
    :rescue, [:resbody, [:array, [:const, :Exception]], [:fcall,
     :puts, [:array, [:dstr, "error while executing rule: ", [
    :evstr]]]]]]]]]],

  "\
  def parse_date(aString)
    return Time.rfc822(aString) rescue Time.parse(aString)
  end" =>
    [[:defn, :parse_date, [:scope, [:block, [:args, :aString], [
    :rescue, [:return, [:call, [:const, :Time], :rfc822, [:array, [
    :lvar, :aString]]]], [:resbody, nil, [:call, [:const, :Time],
     :parse, [:array, [:lvar, :aString]]]]]]]]],

  "\
  def self.has_return_hash_fix?
    rl=RubyLexer.new(\"\",\"return {}.size\")
  end" =>
    [[:defs, [:self], :has_return_hash_fix?, [:scope, [:block, [
    :args], [:lasgn, :rl, [:call, [:const, :RubyLexer], :new, [
    :array, [:str, ""], [:str, "return {}.size"]]]]]]]],

  "\
  def test_endblockwarn
    ruby = EnvUtil.rubybin
    # Use Tempfile to create temporary file path.
    launcher = Tempfile.new(self.class.name)
    errout = Tempfile.new(self.class.name)
    launcher << <<EOF
errout = ARGV.shift
STDERR.reopen(File.open(errout, \"w\"))
STDERR.sync = true
Dir.chdir(\#{q(DIR)})
cmd = \"\\\"\#{ruby}\\\" \\\"endblockwarn.rb\\\"\"
system(cmd)
EOF
    launcher.close
    launcherpath = launcher.path
    errout.close
    erroutpath = errout.path
    system(\"\#{q(ruby)} \#{q(launcherpath)} \#{q(erroutpath)}\")
    expected = <<EOW
endblockwarn.rb:2: warning: END in method; use at_exit
(eval):2: warning: END in method; use at_exit
EOW
    assert_equal(expected, File.read(erroutpath))
    # expecting Tempfile to unlink launcher and errout file.
  end" =>
    [[:defn, :test_endblockwarn, [:scope, [:block, [:args], [
    :lasgn, :ruby, [:call, [:const, :EnvUtil], :rubybin]], [
    :lasgn, :launcher, [:call, [:const, :Tempfile], :new, [:array, [
    :call, [:call, [:self], :class], :name]]]], [:lasgn, :errout, [
    :call, [:const, :Tempfile], :new, [:array, [:call, [:call, [
    :self], :class], :name]]]], [:call, [:lvar, :launcher], :<<,
     [:array, [
    :dstr, "errout = ARGV.shift\nSTDERR.reopen(File.open(errout, \"w\"))\nSTDERR.sync = true\nDir.chdir(", [
    :evstr, [:fcall, :q, [:array, [:const, :DIR]]]], [
    :str, ")\ncmd = \"\""], [:evstr, [:lvar, :ruby]], [
    :str, "\" \"endblockwarn.rb\"\"\nsystem(cmd)\n"]]]], [:call,
     [:lvar, :launcher], :close], [:lasgn, :launcherpath, [:call, [
    :lvar, :launcher], :path]], [:call, [:lvar, :errout], :close], [
    :lasgn, :erroutpath, [:call, [:lvar, :errout], :path]], [
    :fcall, :system, [:array, [:dstr, "", [:evstr, [:fcall, :q, [
    :array, [:lvar, :ruby]]]], [:str, " "], [:evstr, [:fcall, :q, [
    :array, [:lvar, :launcherpath]]]], [:str, " "], [:evstr, [
    :fcall, :q, [:array, [:lvar, :erroutpath]]]]]]], [:lasgn,
     :expected, [
    :str, "endblockwarn.rb:2: warning: END in method; use at_exit\n(eval):2: warning: END in method; use at_exit\n"]], [
    :fcall, :assert_equal, [:array, [:lvar, :expected], [:call, [
    :const, :File], :read, [:array, [:lvar, :erroutpath]]]]]]]]],

  "\
  def tt3(&block)
    tt2(raise(ArgumentError,\"\"),&block)
  end" =>
    [[:defn, :tt3, [:scope, [:block, [:args], [:block_arg, :block], [
    :block_pass, [:lvar, :block], [:fcall, :tt2, [:array, [
    :fcall, :raise, [:array, [:const, :ArgumentError], [:str, ""]
    ]]]]]]]]],

  "\
  if a 
  then b
  else c
  end" =>
    [[:if, [:vcall, :a], [:vcall, :b], [:vcall, :c]]],

  "\
  if a; else c
  end" =>
    [[:if, [:vcall, :a], nil, [:vcall, :c]]],

  "\
  if a; then b
  else c
  end" =>
    [[:if, [:vcall, :a], [:vcall, :b], [:vcall, :c]]],

  "\
  if a;
  then b
  else c
  end" =>
    [[:if, [:vcall, :a], [:vcall, :b], [:vcall, :c]]],

  "\
  if a
  else c
  end" =>
    [[:if, [:vcall, :a], nil, [:vcall, :c]]],

  "\
  if a
  then b else c
  end" =>
    [[:if, [:vcall, :a], [:vcall, :b], [:vcall, :c]]],

  "\
  if
  a
  then
  b
  else
  c
  end" =>
    [[:if, [:vcall, :a], [:vcall, :b], [:vcall, :c]]],

  "\
  if
  a
  then
  b
  end" =>
    [[:if, [:vcall, :a], [:vcall, :b], nil]],

  "  next {}.size" => [[:next, [:call, [:hash], :size]]],
  "\
  p `
  `\

" =>
    [[:fcall, :p, [:array, [:xstr, "\n  "]]]],

  "  p `\\n`" => [[:fcall, :p, [:array, [:xstr, "\n"]]]],
  "\
  p('rb_out', 'args', <<-'EOL')
    regsub -all {!} $args {\\\\!} args
    regsub -all \"{\" $args \"\\\\{\" args
    if {[set st [catch {ruby [format \"TkCore.callback %%Q!%s!\" $args]} ret]] != 0} {
        return -code $st $ret
    } {
        return $ret
    }
  EOL\

" =>
    [[:fcall, :p, [:array, [:str, "rb_out"], [:str, "args"], [
    :str, "    regsub -all {!} $args {\\\\!} args\n    regsub -all \"{\" $args \"\\\\{\" args\n    if {[set st [catch {ruby [format \"TkCore.callback %%Q!%s!\" $args]} ret]] != 0} {\n        return -code $st $ret\n    } {\n        return $ret\n    }\n"]
    ]]],

  "  raise {}.to_s+\"****\"" =>
    [[:call, [:call, [:iter, [:fcall, :raise], nil], :to_s], :+,
     [:array, [:str, "****"]]]],

  "\
  result=[
    MethNameToken.new(old.ident,old.offset),
    ImplicitParamListStartToken.new(input_position),
    ImplicitParamListEndToken.new(input_position),
    *ignored_tokens
   ]" =>
    [[:lasgn, :result, [:argscat, [:array, [:call, [
    :const, :MethNameToken], :new, [:array, [:call, [:vcall, :old],
     :ident], [:call, [:vcall, :old], :offset]]], [:call, [
    :const, :ImplicitParamListStartToken], :new, [:array, [
    :vcall, :input_position]]], [:call, [:const, :ImplicitParamListEndToken], :new, [
    :array, [:vcall, :input_position]]]], [:vcall, :ignored_tokens]
    ]]],

  "  return {}.size" => [[:return, [:call, [:hash], :size]]],
  "  throw {}.to_s+\"****\"" =>
    [[:call, [:call, [:iter, [:fcall, :throw], nil], :to_s], :+,
     [:array, [:str, "****"]]]],

  "\
  undef new_master, new_slave, new_safe_slave
  undef new_trusted_slave, new_safeTk" =>
    [[:block, [:undef, [:lit, :new_master]], [:undef, [
    :lit, :new_slave]], [:undef, [:lit, :new_safe_slave]], [
    :block, [:undef, [:lit, :new_trusted_slave]], [:undef, [:lit,
     :new_safeTk]]]]],

  " (-6000)..476" => [[:dot2, [:lit, -6000], [:lit, 476]]],
  " * =f,g" =>
    [[:masgn, nil, [:splat], [:array, [:vcall, :f], [:vcall, :g]
    ]]],

  " /\\A\#{\"F\"}/" => [[:lit, /\AF/]],
  " /\\A\#{__FILE__}/" => [[:lit, /\A(string)/]],
  " /\\A\#{__FILE__}tcase/n =~ i " =>
    [[:match2, [:lit, /\A(string)tcase/], [:vcall, :i]]],

  " ClassHash[TypeValue = Type, ClassValue = Clalue] " =>
    [[:call, [:const, :ClassHash], :[], [:array, [:cdecl, :TypeValue, [
    :const, :Type]], [:cdecl, :ClassValue, [:const, :Clalue]]]]],

  " ClassHash[TypeValue = Type, ClassValue = Clalue] =1" =>
    [[:attrasgn, [:const, :ClassHash], :[]=, [:array, [:cdecl,
     :TypeValue, [:const, :Type]], [:cdecl, :ClassValue, [:const,
     :Clalue]], [:lit, 1]]]],

  " ClassHash[[TypeValue = Type, ClassValue = Clalue]] " =>
    [[:call, [:const, :ClassHash], :[], [:array, [:array, [
    :cdecl, :TypeValue, [:const, :Type]], [:cdecl, :ClassValue, [
    :const, :Clalue]]]]]],

  " ClassHash[[TypeValue = Type, ClassValue = Clalue]] =1" =>
    [[:attrasgn, [:const, :ClassHash], :[]=, [:array, [:array, [
    :cdecl, :TypeValue, [:const, :Type]], [:cdecl, :ClassValue, [
    :const, :Clalue]]], [:lit, 1]]]],

  " ClassHash[[[TypeValue = Type, ClassValue = Clalue]]] " =>
    [[:call, [:const, :ClassHash], :[], [:array, [:array, [:array, [
    :cdecl, :TypeValue, [:const, :Type]], [:cdecl, :ClassValue, [
    :const, :Clalue]]]]]]],

  " ClassHash[[[TypeValue = Type, ClassValue = Clalue]]] =1" =>
    [[:attrasgn, [:const, :ClassHash], :[]=, [:array, [:array, [
    :array, [:cdecl, :TypeValue, [:const, :Type]], [:cdecl, :ClassValue, [
    :const, :Clalue]]]], [:lit, 1]]]],

  " T.c[:u]=w" =>
    [[:attrasgn, [:call, [:const, :T], :c], :[]=, [:array, [:lit,
     :u], [:vcall, :w]]]],

  " [TypeValue = Type, ClassValue = Clalue] " =>
    [[:array, [:cdecl, :TypeValue, [:const, :Type]], [:cdecl,
     :ClassValue, [:const, :Clalue]]]],

  " [TypeValue, ClassValue = Clalue] " =>
    [[:array, [:const, :TypeValue], [:cdecl, :ClassValue, [
    :const, :Clalue]]]],

  " [[TypeValue = Type, ClassValue = Clalue]] " =>
    [[:array, [:array, [:cdecl, :TypeValue, [:const, :Type]], [
    :cdecl, :ClassValue, [:const, :Clalue]]]]],

  " [[[TypeValue = Type, ClassValue = Clalue]]] " =>
    [[:array, [:array, [:array, [:cdecl, :TypeValue, [:const, :Type]], [
    :cdecl, :ClassValue, [:const, :Clalue]]]]]],

  " a=99;b=3;p 1+(a / b) " =>
    [[:block, [:lasgn, :a, [:lit, 99]], [:lasgn, :b, [:lit, 3]],
     [:fcall, :p, [:array, [:call, [:lit, 1], :+, [:array, [
    :call, [:lvar, :a], :/, [:array, [:lvar, :b]]]]]]]]],

  " begin; rescue ;;error; end" =>
    [[:rescue, [:resbody, nil, [:vcall, :error]]]],

  " c=0;  while c == /[ \\t\\f\\r\\13]/; end " =>
    [[:block, [:lasgn, :c, [:lit, 0]], [:while, [:call, [:lvar,
     :c], :==, [:array, [:lit, /[ \t\f\r\13]/]]], nil, true]]],

  " ca (:f).gg " =>
    [[:fcall, :ca, [:array, [:call, [:lit, :f], :gg]]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  " compile_body=outvar='foob'" =>
    [[:lasgn, :compile_body, [:lasgn, :outvar, [:str, "foob"]]]],

  " false and  ( t) " => [[:and, [:false], [:vcall, :t]]],
  " for bob in [100] do p(bob %(22)) end " =>
    [[:for, [:array, [:lit, 100]], [:lasgn, :bob], [:fcall, :p, [
    :array, [:call, [:lvar, :bob], :%, [:array, [:lit, 22]]]]]]],

  " m.cn= 1, V" =>
    [[:attrasgn, [:vcall, :m], :cn=, [:array, [:svalue, [:array,
     [:lit, 1], [:const, :V]]]]]],

  " mix=nil; p / 5/mix " =>
    [[:block, [:lasgn, :mix, [:nil]], [:call, [:call, [:vcall,
     :p], :/, [:array, [:lit, 5]]], :/, [:array, [:lvar, :mix]]]
    ]],

  " p    pre = $` " =>
    [[:fcall, :p, [:array, [:lasgn, :pre, [:back_ref, :`]]]]],

  " p 1 ? 2 : 3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1 ? 2:3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1 ?2 :3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1 ?2: 3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1 ?2:3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1!=2  " =>
    [[:fcall, :p, [:array, [:not, [:call, [:lit, 1], :==, [:array, [
    :lit, 2]]]]]]],

  " p 1!~2  " =>
    [[:fcall, :p, [:array, [:not, [:call, [:lit, 1], :=~, [:array, [
    :lit, 2]]]]]]],

  " p 1%(2)" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :%, [:array, [:lit,
     2]]]]]],

  " p 1&&2  " =>
    [[:fcall, :p, [:array, [:and, [:lit, 1], [:lit, 2]]]]],

  " p 1&2 " =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :&, [:array, [:lit,
     2]]]]]],

  " p 1**2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :**, [:array, [
    :lit, 2]]]]]],

  " p 1*2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :*, [:array, [:lit,
     2]]]]]],

  " p 1+2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :+, [:array, [:lit,
     2]]]]]],

  " p 1,2  " => [[:fcall, :p, [:array, [:lit, 1], [:lit, 2]]]],
  " p 1-2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :-, [:array, [:lit,
     2]]]]]],

  " p 1...2  " => [[:fcall, :p, [:array, [:lit, 1...2]]]],
  " p 1..2  " => [[:fcall, :p, [:array, [:lit, 1..2]]]],
  " p 1.2" => [[:fcall, :p, [:array, [:lit, 1.2]]]],
  " p 1.class  " =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :class]]]],

  " p 1/2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :/, [:array, [:lit,
     2]]]]]],

  " p 1;2  " =>
    [[:block, [:fcall, :p, [:array, [:lit, 1]]], [:lit, 2]]],

  " p 1<2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :<, [:array, [:lit,
     2]]]]]],

  " p 1<<2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :<<, [:array, [
    :lit, 2]]]]]],

  " p 1<=2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :<=, [:array, [
    :lit, 2]]]]]],

  " p 1<=>2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :<=>, [:array, [
    :lit, 2]]]]]],

  " p 1==2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :==, [:array, [
    :lit, 2]]]]]],

  "\
 p 1==2
 p 1===2
 p 1[2]  #keyword
 p 1;2  #keyword
 p 1,2  #keyword
 p 1.2" =>
    [[:block, [:fcall, :p, [:array, [:call, [:lit, 1], :==, [
    :array, [:lit, 2]]]]], [:fcall, :p, [:array, [:call, [:lit,
     1], :===, [:array, [:lit, 2]]]]], [:fcall, :p, [:array, [
    :call, [:lit, 1], :[], [:array, [:lit, 2]]]]], [:fcall, :p, [
    :array, [:lit, 1]]], [:lit, 2], [:fcall, :p, [:array, [:lit,
     1], [:lit, 2]]], [:fcall, :p, [:array, [:lit, 1.2]]]],
    {:warnings=>["(string):4: warning: useless use of a literal in void context"]}],

  " p 1===2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :===, [:array, [
    :lit, 2]]]]]],

  " p 1=~2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :=~, [:array, [
    :lit, 2]]]]]],

  " p 1>2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :>, [:array, [:lit,
     2]]]]]],

  " p 1>=2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :>=, [:array, [
    :lit, 2]]]]]],

  " p 1>>2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :>>, [:array, [
    :lit, 2]]]]]],

  " p 1? 2 :3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1? 2: 3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1? 2:3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1?2 : 3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1?2 :3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1?2: 3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1?2:3  " =>
    [[:fcall, :p, [:array, [:if, [:lit, 1], [:lit, 2], [:lit, 3]
    ]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  " p 1[2]  " =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :[], [:array, [
    :lit, 2]]]]]],

  " p 1^2" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :^, [:array, [:lit,
     2]]]]]],

  " p 1|2 " =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :|, [:array, [:lit,
     2]]]]]],

  " p 1||2  " =>
    [[:fcall, :p, [:array, [:or, [:lit, 1], [:lit, 2]]]]],

  " p aaa,bbb,ccc=1,2,3  " =>
    [[:fcall, :p, [:array, [:vcall, :aaa], [:vcall, :bbb], [
    :lasgn, :ccc, [:lit, 1]], [:lit, 2], [:lit, 3]]]],

  " p cls_name = {}[:class] " =>
    [[:fcall, :p, [:array, [:lasgn, :cls_name, [:call, [:hash],
     :[], [:array, [:lit, :class]]]]]]],

  " p f = 3.7517675036461267e+17 " =>
    [[:fcall, :p, [:array, [:lasgn, :f, [:lit, 3.751767503646127e+17]
    ]]]],

  " p rescue p ().m" =>
    [[:rescue, [:vcall, :p], [:resbody, nil, [:call, [:fcall, :p],
     :m]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  " p rescue p (4).m" =>
    [[:rescue, [:vcall, :p], [:resbody, nil, [:call, [:fcall, :p, [
    :array, [:lit, 4]]], :m]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  " p rescue p (4,5).m" =>
    [[:rescue, [:vcall, :p], [:resbody, nil, [:call, [:fcall, :p, [
    :array, [:lit, 4], [:lit, 5]]], :m]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  " p rescue p a4.m" =>
    [[:rescue, [:vcall, :p], [:resbody, nil, [:fcall, :p, [:array, [
    :call, [:vcall, :a4], :m]]]]]],

  " p( { :class => class_=0}) " =>
    [[:fcall, :p, [:array, [:hash, [:lit, :class], [:lasgn, :class_, [
    :lit, 0]]]]]],

  " p(/ 1/)" => [[:fcall, :p, [:array, [:lit, / 1/]]]],
  " p(String << - Class)" =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :<<, [
    :array, [:call, [:const, :Class], :-@]]]]]],

  " p(String <<- Class)" =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :<<, [
    :array, [:call, [:const, :Class], :-@]]]]]],

  " p(String >> - Class)" =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :>>, [
    :array, [:call, [:const, :Class], :-@]]]]]],

  " p(String >>- Class)" =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :>>, [
    :array, [:call, [:const, :Class], :-@]]]]]],

  "\
 ret = <<-END
 @@parts_template = \#{template.to_ruby}
 def parts_template
   @@parts_template
 end
 \#{c.const_def_src.join(\"\\n\")}
 def amrita_expand_and_format(element, context, formatter)
   if element.tagname_symbol == :span and element.attrs.size == 0
     amrita_expand_and_format1(element, context, formatter)
   else
     formatter.format_element(element) do
       amrita_expand_and_format1(element, context, formatter)
     end
   end
 end
 def amrita_expand_and_format1(element, context, formatter)
   \#{method_src}
 end
 END\

" =>
    [[:lasgn, :ret, [:dstr, " @@parts_template = ", [:evstr, [
    :call, [:vcall, :template], :to_ruby]], [
    :str, "\n def parts_template\n   @@parts_template\n end\n "], [
    :evstr, [:call, [:call, [:vcall, :c], :const_def_src], :join, [
    :array, [:str, "\n"]]]], [
    :str, "\n def amrita_expand_and_format(element, context, formatter)\n   if element.tagname_symbol == :span and element.attrs.size == 0\n     amrita_expand_and_format1(element, context, formatter)\n   else\n     formatter.format_element(element) do\n       amrita_expand_and_format1(element, context, formatter)\n     end\n   end\n end\n def amrita_expand_and_format1(element, context, formatter)\n   "], [
    :evstr, [:vcall, :method_src]], [:str, "\n end\n"]]]],

  " script=0; @xml.script :type => text/javascript, :src =>\
 \"/javascripts/\#{script}.js\" do end " =>
    [[:block, [:lasgn, :script, [:lit, 0]], [:iter, [:call, [
    :ivar, :@xml], :script, [:array, [:hash, [:lit, :type], [
    :call, [:vcall, :text], :/, [:array, [:vcall, :javascript]]
    ], [:lit, :src], [:dstr, "/javascripts/", [:evstr, [:lvar,
     :script]], [:str, ".js"]]]]], nil]]],

  "! begin end" => [[:not, [:nil]]],
  "!(/a..b/)" =>
    [[:not, [:match, [:lit, /a..b/]]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "!(a..b)" => [[:not, [:flip2, [:vcall, :a], [:vcall, :b]]]],
  "!a" => [[:not, [:vcall, :a]]],
  "!a**2" =>
    [[:call, [:not, [:vcall, :a]], :**, [:array, [:lit, 2]]]],

  "!begin; a; rescue b; end" =>
    [[:not, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]]]],

  "\"\"" => [[:str, ""]],
  "\"\"\"Universal feed parser in Ruby\"\"\";$K=fgdf" =>
    [[:gasgn, :$K, [:vcall, :fgdf]],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "\"\#$A\"" => [[:dstr, "", [:evstr, [:gvar, :$A]]]],
  "\"\#@@A\"" => [[:dstr, "", [:evstr, [:cvar, :@@A]]]],
  "\"\#@A\"" => [[:dstr, "", [:evstr, [:ivar, :@A]]]],
  "\"\#{ * =f,g}\"" =>
    [[:dstr, "", [:evstr, [:masgn, nil, [:splat], [:array, [
    :vcall, :f], [:vcall, :g]]]]]],

  "\"\#{\"\"}\"" => [[:str, ""]],
  "\"\#{\"\#{*a0 = (*a= c)}\"}\"" =>
    [[:dstr, "", [:evstr, [:masgn, nil, [:lasgn, :a0], [:array, [
    :masgn, nil, [:lasgn, :a], [:array, [:vcall, :c]]]]]]]],

  "\"\#{\"foo\"}\"" => [[:str, "foo"]],
  "\"\#{\"foo\"}bar\#{baz}\"" =>
    [[:dstr, "foobar", [:evstr, [:vcall, :baz]]]],

  "\"\#{(\"foo\")}\"" => [[:str, "foo"]],
  "\"\#{()}\"" => [[:dstr, "", [:evstr, [:nil]]]],
  "\"\#{*a0 = (*a= c)}\"" =>
    [[:dstr, "", [:evstr, [:masgn, nil, [:lasgn, :a0], [:array, [
    :masgn, nil, [:lasgn, :a], [:array, [:vcall, :c]]]]]]]],

  "\"\#{*a0= begin a end rescue b0}\"" =>
    [[:dstr, "", [:evstr, [:rescue, [:masgn, nil, [:lasgn, :a0],
     [:array, [:vcall, :a]]], [:resbody, nil, [:vcall, :b0]]]]]],

  "\"\#{*a=b rescue c}\"" =>
    [[:dstr, "", [:evstr, [:rescue, [:masgn, nil, [:lasgn, :a], [
    :array, [:vcall, :b]]], [:resbody, nil, [:vcall, :c]]]]]],

  "\"\#{*a=b}\"" =>
    [[:dstr, "", [:evstr, [:masgn, nil, [:lasgn, :a], [:array, [
    :vcall, :b]]]]]],

  "\"\#{*a[*b]=c}\"" =>
    [[:dstr, "", [:evstr, [:masgn, nil, [:attrasgn, [:vcall, :a],
     :[]=, [:splat, [:vcall, :b]]], [:array, [:vcall, :c]]]]]],

  "\"\#{A}\"" => [[:dstr, "", [:evstr, [:const, :A]]]],
  "\"\#{BEGIN{a}}\"" => [[:dstr, "", [:evstr]]],
  "\"\#{END{a}}\"" =>
    [[:dstr, "", [:evstr, [:iter, [:postexe], nil, [:vcall, :a]]
    ]]],

  "\"\#{__FILE__}\"" => [[:str, "(string)"]],
  "\"\#{__LINE__}\"" => [[:dstr, "", [:evstr, [:lit, 1]]]],
  "\"\#{a,=1}\"" =>
    [[:dstr, "", [:evstr, [:masgn, [:array, [:lasgn, :a]], nil, [
    :to_ary, [:lit, 1]]]]]],

  "\"\#{a=1,2}\"" =>
    [[:dstr, "", [:evstr, [:lasgn, :a, [:svalue, [:array, [:lit,
     1], [:lit, 2]]]]]]],

  "\"\#{a=b,d rescue c}\"" =>
    [[:dstr, "", [:evstr, [:rescue, [:lasgn, :a, [:svalue, [
    :array, [:vcall, :b], [:vcall, :d]]]], [:resbody, nil, [
    :vcall, :c]]]]]],

  "\"\#{a} b c\"" =>
    [[:dstr, "", [:evstr, [:vcall, :a]], [:str, " b c"]]],

  "\"\#{a}\"" => [[:dstr, "", [:evstr, [:vcall, :a]]]],
  "\"\#{a}\#{b}\"" =>
    [[:dstr, "", [:evstr, [:vcall, :a]], [:evstr, [:vcall, :b]]]
    ],

  "\"\#{begin begin; ync; p1; end;rr end}\"" =>
    [[:dstr, "", [:evstr, [:block, [:vcall, :ync], [:vcall, :p1], [
    :vcall, :rr]]]]],

  "\"\#{begin; a; rescue b; end}\"" =>
    [[:dstr, "", [:evstr, [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]]],

  "\"\#{begin}\"" => SyntaxError.new("(string):1: syntax error, unexpected '}'\n\"\#{begin}\"\n         ^\n(string):1: unterminated string meets end of file"),
  "\"\#{false}\"" => [[:dstr, "", [:evstr, [:false]]]],
  "\"\#{nil}\"" => [[:dstr, "", [:evstr, [:nil]]]],
  "\"\#{publi}\#{}>\"" =>
    [[:dstr, "", [:evstr, [:vcall, :publi]], [:evstr], [:str, ">"]
    ]],

  "\"\#{queect{|w| w.t{|w| w}}}\"" =>
    [[:dstr, "", [:evstr, [:iter, [:fcall, :queect], [:dasgn_curr,
     :w], [:iter, [:call, [:dvar, :w], :t], [:dasgn, :w], [:dvar,
     :w]]]]]],

  "\"\#{self}\"" => [[:dstr, "", [:evstr, [:self]]]],
  "\"\#{true}\"" => [[:dstr, "", [:evstr, [:true]]]],
  "\"\#{}\"" => [[:dstr, "", [:evstr]]],
  "\"\#{}\"\"\"" => [[:dstr, "", [:evstr]]],
  "\"1\";a" =>
    [[:vcall, :a],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "\"1\#{2}\";a" =>
    [[:block, [:dstr, "1", [:evstr, [:lit, 2]]], [:vcall, :a]],
    {:warnings=>["(string):1: warning: useless use of a literal in void context"]}],

  "\"1\#{a}2\#{b}\" \"\#{a}2\#{b}3\"" =>
    [[:dstr, "1", [:evstr, [:vcall, :a]], [:str, "2"], [:evstr, [
    :vcall, :b]], [:str, ""], [:evstr, [:vcall, :a]], [:str, "2"], [
    :evstr, [:vcall, :b]], [:str, "3"]]],

  "\"1\#{a}2\#{b}\" \"1\#{a}2\#{b}3\"" =>
    [[:dstr, "1", [:evstr, [:vcall, :a]], [:str, "2"], [:evstr, [
    :vcall, :b]], [:str, "1"], [:evstr, [:vcall, :a]], [:str, "2"], [
    :evstr, [:vcall, :b]], [:str, "3"]]],

  "\"1\#{a}2\#{b}\"\"1\#{a}2\#{b}3\"" =>
    [[:dstr, "1", [:evstr, [:vcall, :a]], [:str, "2"], [:evstr, [
    :vcall, :b]], [:str, "1"], [:evstr, [:vcall, :a]], [:str, "2"], [
    :evstr, [:vcall, :b]], [:str, "3"]]],

  "\"1\#{a}2\#{b}3\"" =>
    [[:dstr, "1", [:evstr, [:vcall, :a]], [:str, "2"], [:evstr, [
    :vcall, :b]], [:str, "3"]]],

  "\"1\#{a}2\#{b}3\" \"\#{a}2\#{b}3\"" =>
    [[:dstr, "1", [:evstr, [:vcall, :a]], [:str, "2"], [:evstr, [
    :vcall, :b]], [:str, "3"], [:str, ""], [:evstr, [:vcall, :a]
    ], [:str, "2"], [:evstr, [:vcall, :b]], [:str, "3"]]],

  "\"1\#{a}2\#{b}3\" \"1\#{a}2\#{b}3\"" =>
    [[:dstr, "1", [:evstr, [:vcall, :a]], [:str, "2"], [:evstr, [
    :vcall, :b]], [:str, "3"], [:str, "1"], [:evstr, [:vcall, :a]
    ], [:str, "2"], [:evstr, [:vcall, :b]], [:str, "3"]]],

  "\"1\#{a}2\#{b}3\"\"1\#{a}2\#{b}3\"" =>
    [[:dstr, "1", [:evstr, [:vcall, :a]], [:str, "2"], [:evstr, [
    :vcall, :b]], [:str, "3"], [:str, "1"], [:evstr, [:vcall, :a]
    ], [:str, "2"], [:evstr, [:vcall, :b]], [:str, "3"]]],

  "\"1\#{begin; r; t end;p (1).m}2\"" =>
    [[:dstr, "1", [:evstr, [:block, [:vcall, :r], [:vcall, :t], [
    :fcall, :p, [:array, [:call, [:lit, 1], :m]]]]], [:str, "2"]
    ],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "\"1\#{p ().m}2\"" =>
    [[:dstr, "1", [:evstr, [:call, [:fcall, :p], :m]], [:str, "2"]
    ],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "\"1\#{p (1).m}2\"" =>
    [[:dstr, "1", [:evstr, [:call, [:fcall, :p, [:array, [:lit,
     1]]], :m]], [:str, "2"]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "\"1\#{p (1,2).m}2\"" =>
    [[:dstr, "1", [:evstr, [:call, [:fcall, :p, [:array, [:lit,
     1], [:lit, 2]]], :m]], [:str, "2"]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "\"1\#{tt;p (1).m}2\"" =>
    [[:dstr, "1", [:evstr, [:block, [:vcall, :tt], [:fcall, :p, [
    :array, [:call, [:lit, 1], :m]]]]], [:str, "2"]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "\"2266\"**\"\#{22}\" \"\#{44}\" \"55\" \"\#{66}\"" =>
    [[:call, [:str, "2266"], :**, [:array, [:dstr, "", [:evstr, [
    :lit, 22]], [:evstr, [:lit, 44]], [:str, "55"], [:evstr, [
    :lit, 66]]]]]],

  "\"__A\#{a=b,c=d}\"" =>
    [[:dstr, "__A", [:evstr, [:lasgn, :a, [:svalue, [:array, [
    :vcall, :b], [:lasgn, :c, [:vcall, :d]]]]]]]],

  "\"\\C-\"\"" => [[:str, "\x02"]],
  "\"\\C-?\"" => [[:str, "\x7F"]],
  "\"\\C-\\\"\"" => [[:str, "\x02"]],
  "\"\\C-\\C-\\C-\\c\\C-a\"" => [[:str, "\x01"]],
  "\"\\C-\\M-\"\"" => [[:str, "\x82"]],
  "\"\\C-\\M-4\"" => [[:str, "\x94"]],
  "\
\"\\C-\\
\"" =>
    [[:str, "\n"]],

  "\"\\C-\\p \"" => [[:str, "\x10 "]],
  "\"\\M-\"\"" => [[:str, "\xA2"]],
  "\"\\M-\\C-\"\"" => [[:str, "\x82"]],
  "\"\\M-\\C-4\"" => [[:str, "\x94"]],
  "\"\\M-\\M-\\M-\\M-\\M-4\"" => [[:str, "\xB4"]],
  "\"\\M-\\M-\\M-\\M-\\c4\"" => [[:str, "\x94"]],
  "\"\\\\M-\\C-?\"" => [[:str, "\\M-\x7F"]],
  "\"\\\\\\\"\"" => [[:str, "\\\""]],
  "\"\\\\\\\\\"" => [[:str, "\\\\"]],
  "\"\\\\\\\\\\\"\"" => [[:str, "\\\\\""]],
  "\"\\\\\\\\\\\\\"" => [[:str, "\\\\\\"]],
  "\"\\c\"\"" => [[:str, "\x02"]],
  "\"\\c6\"" => [[:str, "\x16"]],
  "\
\"\\xa5\\xaa\\xa5\\xd6\\xa5\\xb8\\xa5\\xa7\\xa5\\xaf\\xa5\\xc8\\xbb\\xd8\\xb8\\xfe\\
\\xa5\\xb9\\xa5\\xaf\\xa5\\xea\\xa5\\xd7\\xa5\\xc8\\xb8\\xc0\\xb8\\xec\\
Ruby\"" =>
    [[
    :str, "\xA5\xAA\xA5\xD6\xA5\xB8\xA5\xA7\xA5\xAF\xA5\xC8\xBB\xD8\xB8\xFE\xA5\xB9\xA5\xAF\xA5\xEA\xA5\xD7\xA5\xC8\xB8\xC0\xB8\xECRuby"]
    ],

  "\"\\}\"" => [[:str, "}"]],
  "\"a b c \#{d} \"" =>
    [[:dstr, "a b c ", [:evstr, [:vcall, :d]], [:str, " "]]],

  "\"a b c \#{d}\"" => [[:dstr, "a b c ", [:evstr, [:vcall, :d]]]],
  "\
\"a b c \#{d}
\"" =>
    [[:dstr, "a b c ", [:evstr, [:vcall, :d]], [:str, "\n"]]],

  "\"a b c \#{d}\\n\"" =>
    [[:dstr, "a b c ", [:evstr, [:vcall, :d]], [:str, "\n"]]],

  "\"a\"; b" =>
    [[:vcall, :b],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "\"a\#{m}a\" \"b\#{n}b\"" =>
    [[:dstr, "a", [:evstr, [:vcall, :m]], [:str, "a"], [:str, "b"], [
    :evstr, [:vcall, :n]], [:str, "b"]]],

  "\"a\#{m}a\" \"bb\"" =>
    [[:dstr, "a", [:evstr, [:vcall, :m]], [:str, "a"], [:str, "bb"]
    ]],

  "\"a\#{}\"; b" =>
    [[:block, [:dstr, "a", [:evstr]], [:vcall, :b]],
    {:warnings=>["(string):1: warning: useless use of a literal in void context"]}],

  "\"aa\" \"b\#{n}b\"" =>
    [[:dstr, "aab", [:evstr, [:vcall, :n]], [:str, "b"]]],

  "\"aa\" \"bb\"" => [[:str, "aabb"]],
  "\"f\";p (1..10).method(:each)" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1..10], :method, [
    :array, [:lit, :each]]]]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression", "(string):1: warning: unused literal ignored"]}],

  "\
\"f\"
d" =>
    [[:vcall, :d],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "\"fff\#{r}\" \"\#{66}\"" =>
    [[:dstr, "fff", [:evstr, [:vcall, :r]], [:evstr, [:lit, 66]]
    ]],

  "\"fff\#{r}\" \"\#{66}3\"" =>
    [[:dstr, "fff", [:evstr, [:vcall, :r]], [:str, ""], [:evstr,
     [:lit, 66]], [:str, "3"]]],

  "\"foo\" \"bar\"" => [[:str, "foobar"]],
  "\"foo\" \"bar\#{}\" \"baz\"" =>
    [[:dstr, "foobar", [:evstr], [:str, "baz"]]],

  "\"foo\".slice (1-2).nil?   " =>
    [[:call, [:call, [:str, "foo"], :slice, [:array, [:call, [
    :lit, 1], :-, [:array, [:lit, 2]]]]], :nil?],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "\"foo\#{\"bar\"}\" \"baz\"" => [[:str, "foobarbaz"]],
  "\"foo\#{\"bar\"}quux\#{\"biff\"}\" \"baz\"" =>
    [[:str, "foobarquuxbiffbaz"]],

  "\"foo\#{\"dfg\#{\"sdfsd\"}\" }\"; sdfasd" =>
    [[:vcall, :sdfasd],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "\"foo\#{(\"bar\")}quux\#{\"biff\"}\" \"baz\"" =>
    [[:str, "foobarquuxbiffbaz"]],

  "\"foo\#{(__FILE__)}quux\#{\"biff\"}\" \"baz\"" =>
    [[:str, "foo(string)quuxbiffbaz"]],

  "\"foo\#{bar}quux\" \"baz\"" =>
    [[:dstr, "foo", [:evstr, [:vcall, :bar]], [:str, "quux"], [
    :str, "baz"]]],

  "\"foo\#{bar}quux\#{\"biff\"}\" \"baz\"" =>
    [[:dstr, "foo", [:evstr, [:vcall, :bar]], [:str, "quux"], [
    :str, "biff"], [:str, "baz"]]],

  "\"foo\#{bar}quux\#{biff}\" \"baz\"" =>
    [[:dstr, "foo", [:evstr, [:vcall, :bar]], [:str, "quux"], [
    :evstr, [:vcall, :biff]], [:str, "baz"]]],

  "\"fooo\#{33}fsfd\"" =>
    [[:dstr, "fooo", [:evstr, [:lit, 33]], [:str, "fsfd"]]],

  "\"is \#{\"Slim \#{2?\"W\":\"S\"} \" \"rr\"}.\"" =>
    [[:dstr, "is Slim ", [:evstr, [:if, [:lit, 2], [:str, "W"], [
    :str, "S"]]], [:str, " "], [:str, "rr"], [:str, "."]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "\"is \#{\"Slim \#{2?\"W\":\"S\"} \"}.\"" =>
    [[:dstr, "is Slim ", [:evstr, [:if, [:lit, 2], [:str, "W"], [
    :str, "S"]]], [:str, " "], [:str, "."]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "\"is \#{\"Slim \#{2?\"W\":\"S\"}\"}\#{xx}.\"" =>
    [[:dstr, "is Slim ", [:evstr, [:if, [:lit, 2], [:str, "W"], [
    :str, "S"]]], [:evstr, [:vcall, :xx]], [:str, "."]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "\"is \#{\"Slim \#{2?\"W\":\"S\"}\"}.\"" =>
    [[:dstr, "is Slim ", [:evstr, [:if, [:lit, 2], [:str, "W"], [
    :str, "S"]]], [:str, "."]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "\"is \#{x}\#{\"Slim \#{2?\"W\":\"S\"} \"}.\"" =>
    [[:dstr, "is ", [:evstr, [:vcall, :x]], [:str, "Slim "], [
    :evstr, [:if, [:lit, 2], [:str, "W"], [:str, "S"]]], [:str,
     " "], [:str, "."]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "\"is \#{x}\#{\"Slim \#{2?\"W\":\"S\"}\"}\#{xx}.\"" =>
    [[:dstr, "is ", [:evstr, [:vcall, :x]], [:str, "Slim "], [
    :evstr, [:if, [:lit, 2], [:str, "W"], [:str, "S"]]], [:evstr, [
    :vcall, :xx]], [:str, "."]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "\"is \#{x}\#{%'Slim \#{2?W: S} '}.\"" =>
    [[:dstr, "is ", [:evstr, [:vcall, :x]], [:str, "Slim "], [
    :evstr, [:if, [:lit, 2], [:const, :W], [:const, :S]]], [:str,
     " "], [:str, "."]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "\"is \#{x}\#{%W\"Slim \#{2?\"W\":\"S\"}\"}\#{xx}.\"" =>
    [[:dstr, "is ", [:evstr, [:vcall, :x]], [:evstr, [:array, [
    :str, "Slim"], [:dstr, "", [:evstr, [:if, [:lit, 2], [:str,
     "W"], [:str, "S"]]]]]], [:evstr, [:vcall, :xx]], [:str, "."]
    ],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "\"is \#{x}\#{%r'Slim \#{2?W: S} '}.\"" =>
    [[:dstr, "is ", [:evstr, [:vcall, :x]], [:evstr, [:dregx, "Slim ", [
    :evstr, [:if, [:lit, 2], [:const, :W], [:const, :S]]], [:str,
     " "]]], [:str, "."]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "\"is \#{x}\#{'Slim \#{2?W: S} '}.\"" =>
    [[:dstr, "is ", [:evstr, [:vcall, :x]], [:str, "Slim \#{2?W: S} "], [
    :str, "."]]],

  "\"n\#@t\#{l}\#@m\"" =>
    [[:dstr, "n", [:evstr, [:ivar, :@t]], [:evstr, [:vcall, :l]
    ], [:evstr, [:ivar, :@m]]]],

  "\"sdfe\#{g5}4t\#{(((((__FILE__)))))}dfsd\#{g}\"" =>
    [[:dstr, "sdfe", [:evstr, [:vcall, :g5]], [:str, "4t"], [
    :str, "(string)"], [:str, "dfsd"], [:evstr, [:vcall, :g]]]],

  "\"sdfgfg\#{\"dfgdh\"}\";g" =>
    [[:vcall, :g],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "\
# encoding: utf-8
#\xC3\xA0\xC3\xA8
puts \"text\"
# '\

" =>
    [[:fcall, :puts, [:array, [:str, "text"]]]],

  "\
#-Ku required on command line...
def \xE2\x99\xA5(name)
  puts \"I \xE2\x99\xA5 you, \#{name}!\"
end
\n\xE2\x99\xA5 ARGV.shift
#p ?\xE2\x99\xA5 #not til 1.9
p :\xE2\x99\xA5
#\xE2\x99\xA5
\ndef spade; :sam end
def club; :sangwich end
\n=begin \xE2\x99\xA5
\xE2\x99\xA5\xE2\x99\xA5\xE2\x99\xA5
 \xE2\x99\xA5
=end \xE2\x99\xA5
\nalias heart \xE2\x99\xA5
alias :heart2 :\xE2\x99\xA5
undef \xE2\x99\xA5
alias \xE2\x99\xA5 spade
undef :\xE2\x99\xA5
alias :\xE2\x99\xA5 :club
\n\xE2\x99\xA5 2
\n$\xE2\x99\xA5=2
 
class A\xE2\x99\xA5
  @@\xE2\x99\xA5=3
  def new
    @\xE2\x99\xA5=4
  end
\n  def show
    \xE2\x99\xA5=4
    p $\xE2\x99\xA5,@@\xE2\x99\xA5,@\xE2\x99\xA5,\xE2\x99\xA5
  end
end
\nA\xE2\x99\xA5.new.show
\nalias $heart $\xE2\x99\xA5
\np $heart
\np '\xE2\x99\xA5'
p %w[\xE2\x99\xA5 b c\xE2\x99\xA5 d \xE2\x99\xA5e f\xE2\x99\xA5g]
\n" =>
    SyntaxError.new("(string):2: Invalid char `\\342' in expression\n(string):2: Invalid char `\\231' in expression\n(string):2: Invalid char `\\245' in expression\n(string):2: syntax error, unexpected '\\n', expecting tCOLON2 or '.'\n(string):4: syntax error, unexpected kEND, expecting $end"),

  "\
\#@@ddd=@ddd=$ddd=nil
def DDD;end
def ddd2; \"asdAds\" end
def (DDD()).foofarendle;end
def Integer.foofarendle;end
def @@ddd.foofarendle;  33;end
def @ddd.foofarendle;  33;end
def $ddd.foofarendle;  33;end
def ddd.foofarendle;  33;end
def ddd2.foofarendle;  33;end
def (ddd).foofarendle2; end
def (ddd()).foofarendle2; end
def (ddd2).foofarendle;end
def ddd2.foofarendle;end\

" =>
    [[:block, [:defn, :DDD, [:scope, [:block, [:args], [:nil]]]
    ], [:defn, :ddd2, [:scope, [:block, [:args], [:str, "asdAds"]
    ]]], [:defs, [:fcall, :DDD], :foofarendle, [:scope, [:args]]
    ], [:defs, [:const, :Integer], :foofarendle, [:scope, [:args]
    ]], [:defs, [:cvar, :@@ddd], :foofarendle, [:scope, [:block,
     [:args], [:lit, 33]]]], [:defs, [:ivar, :@ddd], :foofarendle, [
    :scope, [:block, [:args], [:lit, 33]]]], [:defs, [
    :gvar, :$ddd], :foofarendle, [:scope, [:block, [:args], [
    :lit, 33]]]], [:defs, [:vcall, :ddd], :foofarendle, [:scope,
     [:block, [:args], [:lit, 33]]]], [:defs, [:vcall, :ddd2],
     :foofarendle, [:scope, [:block, [:args], [:lit, 33]]]], [
    :defs, [:vcall, :ddd], :foofarendle2, [:scope, [:args]]], [
    :defs, [:fcall, :ddd], :foofarendle2, [:scope, [:args]]], [
    :defs, [:vcall, :ddd2], :foofarendle, [:scope, [:args]]], [
    :defs, [:vcall, :ddd2], :foofarendle, [:scope, [:args]]]]],

  "\
#multiple assignment test
x {
  a,b,c,d,e,f,g,h,i,j,k=1,2,3,4,5,6,7,8,9,10,11
  p(b %(c))
  p(a %(c))
  p(k %(c))
  p(p %(c))
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:masgn, [:array, [
    :dasgn_curr, :a], [:dasgn_curr, :b], [:dasgn_curr, :c], [
    :dasgn_curr, :d], [:dasgn_curr, :e], [:dasgn_curr, :f], [
    :dasgn_curr, :g], [:dasgn_curr, :h], [:dasgn_curr, :i], [
    :dasgn_curr, :j], [:dasgn_curr, :k]], nil, [:array, [:lit, 1], [
    :lit, 2], [:lit, 3], [:lit, 4], [:lit, 5], [:lit, 6], [:lit,
     7], [:lit, 8], [:lit, 9], [:lit, 10], [:lit, 11]]], [:fcall,
     :p, [:array, [:call, [:dvar, :b], :%, [:array, [:dvar, :c]]
    ]]], [:fcall, :p, [:array, [:call, [:dvar, :a], :%, [:array,
     [:dvar, :c]]]]], [:fcall, :p, [:array, [:call, [:dvar, :k],
     :%, [:array, [:dvar, :c]]]]], [:fcall, :p, [:array, [:fcall,
     :p, [:array, [:str, "c"]]]]]]],
    {:warnings=>["(string):7: warning: parenthesize argument(s) for future version"]}],

  "\
#offsets in these cases are incorrect... maybe never
\np <<-foo+'123
  abcd
  foo
  456'+\"8910\"
p heh
\np <<-foo+\"123\#{2.718281828}
  abcd
  foo
  456\#{3.14159265}\"+\"8910\"
p hurhurhurhurhur!
\np <<-foo+\"123\#{2.718281828}
  abcd
  foo
  456\#{3.14159265}\"
p hurhurhurhurhur!
\n
p <<here+'123
456
there
here
789'
p snort!
\np <<here+'123\\
456
there
here
789'
p snort!
\np <<here+'123\\
456
there
here
789'
\np <<-foo+'123
  abcd
  foo
  456'
p ha-ha
\np <<-foo+'123tyurui
  abcdCWECWWE
  foo
  456rtjmnj'
p ha-ha
\n
p \"\#{<<foobar1.each('|'){|s| '\\nthort: '+s}
jbvd|g4543ghb|!@G$dfsd|fafr|e
|s4e5rrwware|BBBBB|*&^(*&^>\"PMK:njs;d|
\nfoobar1
}\"
\n
p <<bazquux+\"\#{
dfgnb
t 67sevrgvase
234vjvj7ui
bazquux
}foobar\"\

" =>
    [[:block, [:fcall, :p, [:array, [:call, [:call, [
    :str, "  abcd\n"], :+, [:array, [:str, "123\n  456"]]], :+, [
    :array, [:str, "8910"]]]]], [:fcall, :p, [:array, [:vcall,
     :heh]]], [:fcall, :p, [:array, [:call, [:call, [
    :str, "  abcd\n"], :+, [:array, [:dstr, "123", [:evstr, [
    :lit, 2.718281828]], [:str, "\n  456"], [:evstr, [
    :lit, 3.14159265]]]]], :+, [:array, [:str, "8910"]]]]], [
    :fcall, :p, [:array, [:fcall, :hurhurhurhurhur!]]], [:fcall,
     :p, [:array, [:call, [:str, "  abcd\n"], :+, [:array, [
    :dstr, "123", [:evstr, [:lit, 2.718281828]], [:str, "\n  456"], [
    :evstr, [:lit, 3.14159265]]]]]]], [:fcall, :p, [:array, [
    :fcall, :hurhurhurhurhur!]]], [:fcall, :p, [:array, [:call, [
    :str, "456\nthere\n"], :+, [:array, [:str, "123\n789"]]]]], [
    :fcall, :p, [:array, [:fcall, :snort!]]], [:fcall, :p, [
    :array, [:call, [:str, "456\nthere\n"], :+, [:array, [:str,
     "123\\\n789"]]]]], [:fcall, :p, [:array, [:fcall, :snort!]]
    ], [:fcall, :p, [:array, [:call, [:str, "456\nthere\n"], :+,
     [:array, [:str, "123\\\n789"]]]]], [:fcall, :p, [:array, [
    :call, [:str, "  abcd\n"], :+, [:array, [:str, "123\n  456"]
    ]]]], [:fcall, :p, [:array, [:call, [:vcall, :ha], :-, [
    :array, [:vcall, :ha]]]]], [:fcall, :p, [:array, [:call, [
    :str, "  abcdCWECWWE\n"], :+, [:array, [:str, "123tyurui\n  456rtjmnj"]]]]], [
    :fcall, :p, [:array, [:call, [:vcall, :ha], :-, [:array, [
    :vcall, :ha]]]]], [:fcall, :p, [:array, [:dstr, "", [:evstr,
     [:iter, [:call, [
    :str, "jbvd|g4543ghb|!@G$dfsd|fafr|e\n|s4e5rrwware|BBBBB|*&^(*&^>\"PMK:njs;d|\n\n"], :each, [
    :array, [:str, "|"]]], [:dasgn_curr, :s], [:call, [
    :str, "\\nthort: "], :+, [:array, [:dvar, :s]]]]]]]], [
    :fcall, :p, [:array, [:call, [:str, "dfgnb\nt 67sevrgvase\n234vjvj7ui\n"], :+, [
    :array, [:dstr, "", [:evstr], [:str, "foobar"]]]]]]]],

  "\
#scope of local variables always includes the here document
#body if it includes the head
p %w[well, whaddaya know].map{|j| <<-END }
\#{j #previous j should be local var, not method
}45634543
END\

" =>
    [[:fcall, :p, [:array, [:iter, [:call, [:array, [:str, "well,"], [
    :str, "whaddaya"], [:str, "know"]], :map], [:dasgn_curr, :j], [
    :dstr, "", [:evstr, [:dvar, :j]], [:str, "45634543\n"]]]]]],

  "\
#test variable creation in string inclusion
#currently broken because string inclusions
#are lexed by a separate lexer!
proc {
  p \"jentawz: \#{baz=200}\"
  p( baz %(9))
}.call\

" =>
    [[:call, [:iter, [:fcall, :proc], nil, [:block, [:fcall, :p,
     [:array, [:dstr, "jentawz: ", [:evstr, [:dasgn_curr, :baz, [
    :lit, 200]]]]]], [:fcall, :p, [:array, [:call, [:dvar, :baz],
     :%, [:array, [:lit, 9]]]]]]], :call]],

  "$$" => [[:gvar, :$$]],
  "$&" => [[:back_ref, :&]],
  "$'" => [[:back_ref, :"'"]],
  "$*" => [[:gvar, :$*]],
  "$+" => [[:back_ref, :+]],
  "$-0" => [[:gvar, :$-0]],
  "$-1" => [[:gvar, :$-1]],
  "$-9" => [[:gvar, :$-9]],
  "$-[]" => [[:call, [:gvar, :"$-"], :[]]],
  "$-\v" => [[:gvar, :"$-"]],
  "$-a" => [[:gvar, :$-a]],
  "$0" => [[:gvar, :$0]],
  "$1" => [[:nth_ref, 1]],
  "$11" => [[:nth_ref, 11]],
  "$111" => [[:nth_ref, 111]],
  "$111111" => [[:nth_ref, 111111]],
  "$11111111111111111111111111111111111111111111111111111111111111111111" =>
    [[:nth_ref, -1]],

  "$2" => [[:nth_ref, 2]],
  "$:" => [[:gvar, :$:]],
  "$;" => [[:gvar, :$;]],
  "$;=1" => [[:gasgn, :$;, [:lit, 1]]],
  "$`" => [[:back_ref, :`]],
  "$a" => [[:gvar, :$a]],
  "$a+=b" =>
    [[:gasgn, :$a, [:call, [:gvar, :$a], :+, [:array, [:vcall, :b]
    ]]]],

  "$a=b" => [[:gasgn, :$a, [:vcall, :b]]],
  "$a||=b" =>
    [[:op_asgn_or, [:gvar, :$a], [:gasgn, :$a, [:vcall, :b]]]],

  "$foo::Bar" => [[:colon2, [:gvar, :$foo], :Bar]],
  "\
% foo  
%\tfoo\t 
%\vfoo\v 
%\rfoo\r 
%
foo
 
%\x00foo\x00 
%
foo
 
%
\rfoo
\r 
%
\rfoo
 
%
foo
 
%\rfoo\r 
%
foo
 
%
foo
 \

" =>
    [[:str, "foo"],
    {:warnings=>["(string):1: warning: unused literal ignored", "(string):2: warning: unused literal ignored", "(string):3: warning: unused literal ignored", "(string):4: warning: unused literal ignored", "(string):6: warning: unused literal ignored", "(string):8: warning: unused literal ignored", "(string):10: warning: unused literal ignored", "(string):13: warning: unused literal ignored", "(string):16: warning: unused literal ignored", "(string):19: warning: unused literal ignored", "(string):21: warning: unused literal ignored", "(string):23: warning: unused literal ignored"]}],

  "%\"is \#{x}\#{%r'Slim \#{2?W: S} '}.\"" =>
    [[:dstr, "is ", [:evstr, [:vcall, :x]], [:evstr, [:dregx, "Slim ", [
    :evstr, [:if, [:lit, 2], [:const, :W], [:const, :S]]], [:str,
     " "]]], [:str, "."]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%$\#$_$" => [[:dstr, "", [:evstr, [:gvar, :$_]]]],
  "%(((\#{}(()))))" => [[:dstr, "((", [:evstr], [:str, "(())))"]]],
  "%(((((\#{})))))" => [[:dstr, "((((", [:evstr], [:str, "))))"]]],
  "%((((())\#{})))" => [[:dstr, "(((())", [:evstr], [:str, "))"]]],
  "%@\#@_@" => [[:dstr, "", [:evstr, [:ivar, :@_]]]],
  "%Q\"\#{}\"" => [[:dstr, "", [:evstr]]],
  "%Q\"a\"" => [[:str, "a"]],
  "%Q[\\|\\|]" => [[:str, "||"]],
  "%W\" \#{} \"" => [[:array, [:dstr, "", [:evstr]]]],
  "%W\" \#{}\"" => [[:array, [:dstr, "", [:evstr]]]],
  "%W\"\#{} \"" => [[:array, [:dstr, "", [:evstr]]]],
  "%W\"\#{}\"" => [[:array, [:dstr, "", [:evstr]]]],
  "%W\"a \#{}\"" => [[:array, [:str, "a"], [:dstr, "", [:evstr]]]],
  "%W\"a\"" => [[:array, [:str, "a"]]],
  "%W\"a\"; b" => [[:block, [:array, [:str, "a"]], [:vcall, :b]]],
  "%W\"a\#{}\"" => [[:array, [:dstr, "a", [:evstr]]]],
  "%W\"is \#{\"Slim \#{2?\"W\":\"S\"} \" \"rr\"}.\"" =>
    [[:array, [:str, "is"], [:dstr, "Slim ", [:evstr, [:if, [
    :lit, 2], [:str, "W"], [:str, "S"]]], [:str, " "], [:str, "rr"], [
    :str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{\"Slim \#{2?\"W\":\"S\"} \"}.\"" =>
    [[:array, [:str, "is"], [:dstr, "Slim ", [:evstr, [:if, [
    :lit, 2], [:str, "W"], [:str, "S"]]], [:str, " "], [:str, "."]
    ]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{\"Slim \#{2?\"W\":\"S\"}\"}\#{xx}.\"" =>
    [[:array, [:str, "is"], [:dstr, "Slim ", [:evstr, [:if, [
    :lit, 2], [:str, "W"], [:str, "S"]]], [:evstr, [:vcall, :xx]
    ], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{\"Slim \#{2?\"W\":\"S\"}\"}.\"" =>
    [[:array, [:str, "is"], [:dstr, "Slim ", [:evstr, [:if, [
    :lit, 2], [:str, "W"], [:str, "S"]]], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{x}\#{\"Slim \#{2?\"W\":\"S\"} \"}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :str, "Slim "], [:evstr, [:if, [:lit, 2], [:str, "W"], [:str,
     "S"]]], [:str, " "], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{x}\#{\"Slim \#{2?\"W\":\"S\"}\"}\#{xx}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :str, "Slim "], [:evstr, [:if, [:lit, 2], [:str, "W"], [:str,
     "S"]]], [:evstr, [:vcall, :xx]], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{x}\#{\"Slim \#{2?W: S} \"}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :str, "Slim "], [:evstr, [:if, [:lit, 2], [:const, :W], [
    :const, :S]]], [:str, " "], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{x}\#{%Q'Slim \#{2?W: S} '}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :str, "Slim "], [:evstr, [:if, [:lit, 2], [:const, :W], [
    :const, :S]]], [:str, " "], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{x}\#{%W\"Slim \#{2?\"W\":\"S\"}\"}\#{xx}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :evstr, [:array, [:str, "Slim"], [:dstr, "", [:evstr, [:if, [
    :lit, 2], [:str, "W"], [:str, "S"]]]]]], [:evstr, [:vcall, :xx]
    ], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{x}\#{%W'Slim \#{2?W: S} '}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :evstr, [:array, [:str, "Slim"], [:dstr, "", [:evstr, [:if, [
    :lit, 2], [:const, :W], [:const, :S]]]]]], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{x}\#{%q'Slim \#{2?W: S} '}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :str, "Slim \#{2?W: S} "], [:str, "."]]]],

  "%W\"is \#{x}\#{%r'Slim \#{2?W: S} '}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :evstr, [:dregx, "Slim ", [:evstr, [:if, [:lit, 2], [:const,
     :W], [:const, :S]]], [:str, " "]]], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{x}\#{%w'Slim \#{2?W: S} '}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :evstr, [:array, [:str, "Slim"], [:str, "\#{2?W:"], [:str,
     "S}"]]], [:str, "."]]]],

  "%W\"is \#{x}\#{%x'Slim \#{2?W: S} '}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :evstr, [:dxstr, "Slim ", [:evstr, [:if, [:lit, 2], [:const,
     :W], [:const, :S]]], [:str, " "]]], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{x}\#{/Slim \#{2?W: S} /}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :evstr, [:dregx, "Slim ", [:evstr, [:if, [:lit, 2], [:const,
     :W], [:const, :S]]], [:str, " "]]], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is \#{x}\#{`Slim \#{2?W: S} `}.\"" =>
    [[:array, [:str, "is"], [:dstr, "", [:evstr, [:vcall, :x]], [
    :evstr, [:dxstr, "Slim ", [:evstr, [:if, [:lit, 2], [:const,
     :W], [:const, :S]]], [:str, " "]]], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is_\#{\"Slim_\#{2?\"W\":\"S\"}\"}\#{xx}.\"" =>
    [[:array, [:dstr, "is_Slim_", [:evstr, [:if, [:lit, 2], [
    :str, "W"], [:str, "S"]]], [:evstr, [:vcall, :xx]], [:str, "."]
    ]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is_\#{\"Slim_\#{2?\"W\":\"S\"}\"}.\"" =>
    [[:array, [:dstr, "is_Slim_", [:evstr, [:if, [:lit, 2], [
    :str, "W"], [:str, "S"]]], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is_\#{\"Slim_\#{2?\"W\":\"S\"}_\" \"rr\"}.\"" =>
    [[:array, [:dstr, "is_Slim_", [:evstr, [:if, [:lit, 2], [
    :str, "W"], [:str, "S"]]], [:str, "_"], [:str, "rr"], [:str,
     "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is_\#{\"Slim_\#{2?\"W\":\"S\"}_\"}.\"" =>
    [[:array, [:dstr, "is_Slim_", [:evstr, [:if, [:lit, 2], [
    :str, "W"], [:str, "S"]]], [:str, "_"], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is_\#{x}\#{\"Slim_\#{2?\"W\":\"S\"}\"}\#{xx}.\"" =>
    [[:array, [:dstr, "is_", [:evstr, [:vcall, :x]], [:str, "Slim_"], [
    :evstr, [:if, [:lit, 2], [:str, "W"], [:str, "S"]]], [:evstr, [
    :vcall, :xx]], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W\"is_\#{x}\#{\"Slim_\#{2?\"W\":\"S\"}_\"}.\"" =>
    [[:array, [:dstr, "is_", [:evstr, [:vcall, :x]], [:str, "Slim_"], [
    :evstr, [:if, [:lit, 2], [:str, "W"], [:str, "S"]]], [:str,
     "_"], [:str, "."]]],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "%W(white\\  \\  \\ \\  \\ space).should == [\"white \", \"\
 \", \"  \", \" space\"]" =>
    [[:call, [:call, [:array, [:str, "white "], [:str, " "], [
    :str, "  "], [:str, " space"]], :should], :==, [:array, [
    :array, [:str, "white "], [:str, " "], [:str, "  "], [:str,
     " space"]]]]],

  "%W[ \#{}]" => [[:array, [:dstr, "", [:evstr]]]],
  "%W[ ]" => [[:zarray]],
  "%W[ a \#{}]" => [[:array, [:str, "a"], [:dstr, "", [:evstr]]]],
  "%W[ a ]" => [[:array, [:str, "a"]]],
  "%W[ a\#{}]" => [[:array, [:dstr, "a", [:evstr]]]],
  "%W[ a]" => [[:array, [:str, "a"]]],
  "%W[\#{\"b\"}c]" => [[:array, [:str, "bc"]]],
  "\
%W[\#{G}
 Z]" =>
    [[:array, [:dstr, "", [:evstr, [:const, :G]]], [:str, "Z"]]],

  "%W[\#{a} b]" =>
    [[:array, [:dstr, "", [:evstr, [:vcall, :a]]], [:str, "b"]]],

  "%W[\#{a}]" => [[:array, [:dstr, "", [:evstr, [:vcall, :a]]]]],
  "%W[Ml.\#{G.ma}.Z M.\#{G.m} yaml.Z ]" =>
    [[:array, [:dstr, "Ml.", [:evstr, [:call, [:const, :G], :ma]
    ], [:str, ".Z"]], [:dstr, "M.", [:evstr, [:call, [:const, :G],
     :m]]], [:str, "yaml.Z"]]],

  "%W[\v]" => [[:zarray]],
  "%W[\va]" => [[:array, [:str, "a"]]],
  "%W[]" => [[:zarray]],
  "%W[\\ \#{}]" => [[:array, [:dstr, " ", [:evstr]]]],
  "%W[\\ ]" => [[:array, [:str, " "]]],
  "%W[\\\"\#{}]" => [[:array, [:dstr, "\"", [:evstr]]]],
  "%W[\\\"]" => [[:array, [:str, "\""]]],
  "%W[\\#\#{}]" => [[:array, [:dstr, "#", [:evstr]]]],
  "%W[\\#]" => [[:array, [:str, "#"]]],
  "%W[\\C-a\#{}]" => [[:array, [:dstr, "\x01", [:evstr]]]],
  "%W[\\C-a]" => [[:array, [:str, "\x01"]]],
  "%W[\\\t\#{}]" => [[:array, [:dstr, "\t", [:evstr]]]],
  "%W[\\\t]" => [[:array, [:str, "\t"]]],
  "%W[\\\v]" => [[:array, [:str, "\v"]]],
  "%W[\\\\\#{}]" => [[:array, [:dstr, "\\", [:evstr]]]],
  "%W[\\\\]" => [[:array, [:str, "\\"]]],
  "%W[\\n\#{}]" => [[:array, [:dstr, "\n", [:evstr]]]],
  "%W[\\n]" => [[:array, [:str, "\n"]]],
  "%W[\\q\#{}]" => [[:array, [:dstr, "q", [:evstr]]]],
  "%W[\\q]" => [[:array, [:str, "q"]]],
  "%W[\\|\\|]" => [[:array, [:str, "||"]]],
  "%W[a \#{b} c]" =>
    [[:array, [:str, "a"], [:dstr, "", [:evstr, [:vcall, :b]]], [
    :str, "c"]]],

  "%W[a \#{b}]" =>
    [[:array, [:str, "a"], [:dstr, "", [:evstr, [:vcall, :b]]]]],

  "%W[a \#{b}c]" =>
    [[:array, [:str, "a"], [:dstr, "", [:evstr, [:vcall, :b]], [
    :str, "c"]]]],

  "%W[a \#{}]" => [[:array, [:str, "a"], [:dstr, "", [:evstr]]]],
  "%W[a ]" => [[:array, [:str, "a"]]],
  "%W[a b \#{c}]" =>
    [[:array, [:str, "a"], [:str, "b"], [:dstr, "", [:evstr, [
    :vcall, :c]]]]],

  "%W[a b c\#{}]" =>
    [[:array, [:str, "a"], [:str, "b"], [:dstr, "c", [:evstr]]]],

  "%W[a b c]" => [[:array, [:str, "a"], [:str, "b"], [:str, "c"]]],
  "%W[a b\#{}]" => [[:array, [:str, "a"], [:dstr, "b", [:evstr]]]],
  "%W[a b]" => [[:array, [:str, "a"], [:str, "b"]]],
  "%W[a\#{\" b  \"}]" => [[:array, [:str, "a b  "]]],
  "%W[a\#{\"b\"}]" => [[:array, [:str, "ab"]]],
  "%W[a\#{\"b\"}c\#{__FILE__}e\#{\"f\"}]" =>
    [[:array, [:str, "abc(string)ef"]]],

  "%W[a\#{\"b\"}c]" => [[:array, [:str, "abc"]]],
  "%W[a\#{b} c]" =>
    [[:array, [:dstr, "a", [:evstr, [:vcall, :b]]], [:str, "c"]]
    ],

  "%W[a\#{b}c]" =>
    [[:array, [:dstr, "a", [:evstr, [:vcall, :b]], [:str, "c"]]]
    ],

  "%W[a\#{}]" => [[:array, [:dstr, "a", [:evstr]]]],
  "%W[a]" => [[:array, [:str, "a"]]],
  "%W[a\\\\c  b]" => [[:array, [:str, "a\\c"], [:str, "b"]]],
  "%W[a\\\\c b]" => [[:array, [:str, "a\\c"], [:str, "b"]]],
  "%W[a\\c  b]" => [[:array, [:str, "a\x00"], [:str, "b"]]],
  "%W[a\\c b]" => [[:array, [:str, "a\x00b"]]],
  "%W[r\\c     3]" => [[:array, [:str, "r\x00"], [:str, "3"]]],
  "%[\\\"]" => [[:str, "\""]],
  "%[a b c]" => [[:str, "a b c"]],
  "%[a b]" => [[:str, "a b"]],
  "%[a\#{\"b\"}c\#{__FILE__}e\#{\"f\"}]" =>
    [[:str, "abc(string)ef"]],

  "%[a]" => [[:str, "a"]],
  "\
%
__END__
[a]" =>
    [[:call, [:str, "__END__"], :[], [:array, [:vcall, :a]]]],

  "\
%
\\c
yy\

" =>
    [[:str, "\nyy"]],

  "\
%
sdfsdsdfsdfsd
\n
\n\

" =>
    [[:str, "sdfsdsdfsdfsd"]],

  "%q\"\#{}\"" => [[:str, "\#{}"]],
  "%q\"\\\"\\\\\"" => [[:str, "\"\\"]],
  "%q\"a\"" => [[:str, "a"]],
  "%q'\\'\\\\'" => [[:str, "'\\"]],
  "%q[\\']" => [[:str, "\\'"]],
  "%q[\\[\\]\\\\]" => [[:str, "[]\\"]],
  "%q[\\|\\|]" => [[:str, "\\|\\|"]],
  "%q[a b c]" => [[:str, "a b c"]],
  "%q[a b]" => [[:str, "a b"]],
  "%q[a]" => [[:str, "a"]],
  "%q[asdfadfas\\']" => [[:str, "asdfadfas\\'"]],
  "%r\"\#{}\"" => [[:dregx, "", [:evstr]]],
  "%r\"\\\"\\\\\"" => [[:lit, /\"\\/]],
  "%r\"a\"" => [[:lit, /a/]],
  "%r'\\'\\\\'" => [[:lit, /\'\\/]],
  "%r[\\[\\]\\\\]" => [[:lit, /\[\]\\/]],
  "%r[\\|\\|]" => [[:lit, /\|\|/]],
  "%s\"\\\"\\\\\"" => [[:lit, :"\"\\"]],
  "%s'\\'\\\\'" => [[:lit, :"'\\"]],
  "%s[\\[\\]\\\\]" => [[:lit, :"[]\\"]],
  "%s[\\|\\|]" => [[:lit, :"\\|\\|"]],
  "%w![ ] { } ( ) | - * . \\\\ ? + ^ $ #!" =>
    [[:array, [:str, "["], [:str, "]"], [:str, "{"], [:str, "}"], [
    :str, "("], [:str, ")"], [:str, "|"], [:str, "-"], [:str, "*"], [
    :str, "."], [:str, "\\"], [:str, "?"], [:str, "+"], [:str, "^"], [
    :str, "$"], [:str, "#"]]],

  "%w![ ] { } ( ) | - * . \\\\\\\\ ? + ^ $ #!" =>
    [[:array, [:str, "["], [:str, "]"], [:str, "{"], [:str, "}"], [
    :str, "("], [:str, ")"], [:str, "|"], [:str, "-"], [:str, "*"], [
    :str, "."], [:str, "\\\\"], [:str, "?"], [:str, "+"], [:str,
     "^"], [:str, "$"], [:str, "#"]]],

  "%w\"\#{}\"" => [[:array, [:str, "\#{}"]]],
  "%w\"\\\"\\\\\"" => [[:array, [:str, "\"\\"]]],
  "%w\"a\"" => [[:array, [:str, "a"]]],
  "%w\"a\"; b" => [[:block, [:array, [:str, "a"]], [:vcall, :b]]],
  "%w'\\'\\\\'" => [[:array, [:str, "'\\"]]],
  "%w(1067595299  955945823  477289528 4107218783 4228976476)" =>
    [[:array, [:str, "1067595299"], [:str, "955945823"], [:str,
     "477289528"], [:str, "4107218783"], [:str, "4228976476"]]],

  "\
%w(
    CVS SCCS)" =>
    [[:array, [:str, "CVS"], [:str, "SCCS"]]],

  "%w[  a b]" => [[:array, [:str, "a"], [:str, "b"]]],
  "%w[ ]" => [[:zarray]],
  "%w[- \\\\ ]" => [[:array, [:str, "-"], [:str, "\\"]]],
  "%w[- \\\\ e]" =>
    [[:array, [:str, "-"], [:str, "\\"], [:str, "e"]]],

  "%w[- \\\\\\\\ ]" => [[:array, [:str, "-"], [:str, "\\\\"]]],
  "%w[- \\\\\\\\ e]" =>
    [[:array, [:str, "-"], [:str, "\\\\"], [:str, "e"]]],

  "%w[\v]" => [[:zarray]],
  "%w[]" => [[:zarray]],
  "%w[\\ ]" => [[:array, [:str, " "]]],
  "%w[\\[\\]\\\\]" => [[:array, [:str, "[]\\"]]],
  "%w[\\\t]" => [[:array, [:str, "\t"]]],
  "%w[\\\v]" => [[:array, [:str, "\v"]]],
  "%w[\\|\\|]" => [[:array, [:str, "\\|\\|"]]],
  "%w[a b c]" => [[:array, [:str, "a"], [:str, "b"], [:str, "c"]]],
  "%w[a b]" => [[:array, [:str, "a"], [:str, "b"]]],
  "%w[a]" => [[:array, [:str, "a"]]],
  "%w[a\\C- b]" => [[:array, [:str, "a\\C-"], [:str, "b"]]],
  "%w[a\\M- b]" => [[:array, [:str, "a\\M-"], [:str, "b"]]],
  "%w[a\\\\c  b]" => [[:array, [:str, "a\\c"], [:str, "b"]]],
  "%w[a\\\\c b]" => [[:array, [:str, "a\\c"], [:str, "b"]]],
  "%w[a\\c  b]" => [[:array, [:str, "a\\c"], [:str, "b"]]],
  "%w[a\\c b]" => [[:array, [:str, "a\\c"], [:str, "b"]]],
  "%x\"\#{}\"" => [[:dxstr, "", [:evstr]]],
  "%x\"a\"" => [[:xstr, "a"]],
  "%x[\\|\\|]" => [[:xstr, "||"]],
  "%{\\C-\"}" => [[:str, "\x02"]],
  "%{\\C-\\\"}" => [[:str, "\x02"]],
  "%{\\C-\\\\}" => [[:str, "\x1C"]],
  "%{\\C-\\{}" => [[:str, "\e"]],
  "%{\\C-\\}}" => [[:str, "\x1D"]],
  "%{\\}}" => [[:str, "}"]],
  "''" => [[:str, ""]],
  "'\\'\\\\'" => [[:str, "'\\"]],
  "'a'" => [[:str, "a"]],
  "'a'; b" =>
    [[:vcall, :b],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "\
(%
eee\#{kk}
)" =>
    [[:dstr, "eee", [:evstr, [:vcall, :kk]]]],

  "\
(%
eee\#{kk}\\
\n)" =>
    [[:dstr, "eee", [:evstr, [:vcall, :kk]], [:str, ""]]],

  "((((*a=c))))" =>
    [[:masgn, nil, [:lasgn, :a], [:array, [:vcall, :c]]]],

  "(((*a=c)))" =>
    [[:masgn, nil, [:lasgn, :a], [:array, [:vcall, :c]]]],

  "(((a,b)))=c,d" =>
    [[:masgn, [:array, [:masgn, [:array, [:masgn, [:array, [
    :lasgn, :a], [:lasgn, :b]], nil, nil]], nil, nil]], nil, [
    :array, [:vcall, :c], [:vcall, :d]]]],

  "((*a=c))" =>
    [[:masgn, nil, [:lasgn, :a], [:array, [:vcall, :c]]]],

  "((a,b))=c,d" =>
    [[:masgn, [:array, [:masgn, [:array, [:lasgn, :a], [:lasgn,
     :b]], nil, nil]], nil, [:array, [:vcall, :c], [:vcall, :d]]
    ]],

  "((m).kk,(m+n).kk),t=3" =>
    [[:masgn, [:array, [:masgn, [:array, [:attrasgn, [:vcall, :m],
     :kk=], [:attrasgn, [:call, [:vcall, :m], :+, [:array, [
    :vcall, :n]]], :kk=]], nil, nil], [:lasgn, :t]], nil, [
    :to_ary, [:lit, 3]]]],

  "((m).kk,(m+n).kk)=3" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :m], :kk=], [:attrasgn, [
    :call, [:vcall, :m], :+, [:array, [:vcall, :n]]], :kk=]], nil, [
    :to_ary, [:lit, 3]]]],

  "()" => [[:nil]],
  "() until 1" =>
    [[:until, [:lit, 1], [:nil], true],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "();a" =>
    [[:block, [:nil], [:vcall, :a]],
    {:warnings=>["(string):1: warning: useless use of nil in void context"]}],

  "(*)=f,g" =>
    [[:masgn, nil, [:splat], [:array, [:vcall, :f], [:vcall, :g]
    ]]],

  "(*a)=1" => [[:masgn, nil, [:lasgn, :a], [:array, [:lit, 1]]]],
  "(*a)=b" =>
    [[:masgn, nil, [:lasgn, :a], [:array, [:vcall, :b]]]],

  "(*a=c)" =>
    [[:masgn, nil, [:lasgn, :a], [:array, [:vcall, :c]]]],

  "(/ 1/)" => [[:lit, / 1/]],
  "(/ 1/);p" =>
    [[:vcall, :p],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "(;1;2;)" =>
    [[:lit, 2],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "(;1;2;);p" =>
    [[:vcall, :p],
    {:warnings=>["(string):1: warning: unused literal ignored", "(string):1: warning: unused literal ignored"]}],

  "(BEGIN {})" => [[:nil]],
  "(a && b) and c" =>
    [[:and, [:and, [:vcall, :a], [:vcall, :b]], [:vcall, :c]]],

  "(a and b) && c" =>
    [[:and, [:and, [:vcall, :a], [:vcall, :b]], [:vcall, :c]]],

  "(a or b) || c" =>
    [[:or, [:or, [:vcall, :a], [:vcall, :b]], [:vcall, :c]]],

  "(a || b) or c" =>
    [[:or, [:or, [:vcall, :a], [:vcall, :b]], [:vcall, :c]]],

  "(a) until 1" =>
    [[:until, [:lit, 1], [:vcall, :a], true],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "(a) while l 1" =>
    [[:while, [:fcall, :l, [:array, [:lit, 1]]], [:vcall, :a],
     true]],

  "(a).b" => [[:call, [:vcall, :a], :b]],
  "(a).b do end" => [[:iter, [:call, [:vcall, :a], :b], nil]],
  "(a+b)" => [[:call, [:vcall, :a], :+, [:array, [:vcall, :b]]]],
  "(a,)=b" =>
    [[:masgn, [:array, [:lasgn, :a]], nil, [:to_ary, [:vcall, :b]
    ]]],

  "(a,)=b rescue c" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :a]], nil, [:to_ary, [
    :vcall, :b]]], [:resbody, nil, [:vcall, :c]]]],

  "(a,*)=b rescue c" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :a]], [:splat], [
    :to_ary, [:vcall, :b]]], [:resbody, nil, [:vcall, :c]]]],

  "(a,*d)=b rescue c" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :a]], [:lasgn, :d], [
    :to_ary, [:vcall, :b]]], [:resbody, nil, [:vcall, :c]]]],

  "(a,b),c=[1,2],3" =>
    [[:masgn, [:array, [:masgn, [:array, [:lasgn, :a], [:lasgn,
     :b]], nil, nil], [:lasgn, :c]], nil, [:array, [:array, [
    :lit, 1], [:lit, 2]], [:lit, 3]]]],

  "(a,b)=c,d" =>
    [[:masgn, [:array, [:lasgn, :a], [:lasgn, :b]], nil, [:array, [
    :vcall, :c], [:vcall, :d]]]],

  "(a,d)=b rescue c" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :a], [:lasgn, :d]], nil, [
    :to_ary, [:vcall, :b]]], [:resbody, nil, [:vcall, :c]]]],

  "(a.b.c).b do end" =>
    [[:iter, [:call, [:call, [:call, [:vcall, :a], :b], :c], :b],
     nil]],

  "(a0,)= begin a end rescue b0" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :a0]], nil, [:to_ary, [
    :vcall, :a]]], [:resbody, nil, [:vcall, :b0]]]],

  "(a;b;c);d;e;f;(g;h;i)" =>
    [[:block, [:vcall, :a], [:vcall, :b], [:vcall, :c], [:vcall,
     :d], [:vcall, :e], [:vcall, :f], [:block, [:vcall, :g], [
    :vcall, :h], [:vcall, :i]]]],

  "(a=b) rescue c" =>
    [[:rescue, [:lasgn, :a, [:vcall, :b]], [:resbody, nil, [
    :vcall, :c]]]],

  "(begin; a; rescue b; end).m" =>
    [[:call, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], :m]],

  "(begin; r; t end;b;c);d;e;f;(g;h;i)" =>
    [[:block, [:vcall, :r], [:vcall, :t], [:vcall, :b], [:vcall,
     :c], [:vcall, :d], [:vcall, :e], [:vcall, :f], [:block, [
    :vcall, :g], [:vcall, :h], [:vcall, :i]]]],

  "(m).kk,(m+n).kk=3" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :m], :kk=], [:attrasgn, [
    :call, [:vcall, :m], :+, [:array, [:vcall, :n]]], :kk=]], nil, [
    :to_ary, [:lit, 3]]]],

  "(mkk,(a=b,c).kk)=3" =>
    [[:masgn, [:array, [:lasgn, :mkk], [:attrasgn, [:lasgn, :a, [
    :svalue, [:array, [:vcall, :b], [:vcall, :c]]]], :kk=]], nil, [
    :to_ary, [:lit, 3]]]],

  "(size-1).downto(0){|i| expr=self[i]}" =>
    [[:iter, [:call, [:call, [:vcall, :size], :-, [:array, [:lit,
     1]]], :downto, [:array, [:lit, 0]]], [:dasgn_curr, :i], [
    :dasgn_curr, :expr, [:fcall, :[], [:array, [:dvar, :i]]]]]],

  "(z[begin; a; rescue b; end],)=c" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :z], :[]=, [:array, [
    :rescue, [:vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]
    ], nil, [:to_ary, [:vcall, :c]]]],

  "(z[begin; a; rescue b; end],*)=c" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :z], :[]=, [:array, [
    :rescue, [:vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]
    ], [:splat], [:to_ary, [:vcall, :c]]]],

  "(z[begin; a; rescue b; end],*f),g=c" =>
    [[:masgn, [:array, [:masgn, [:array, [:attrasgn, [:vcall, :z],
     :[]=, [:array, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]]]]], [:lasgn, :f], nil], [:lasgn, :g]], nil, [
    :to_ary, [:vcall, :c]]]],

  "* = z" => [[:masgn, nil, [:splat], [:array, [:vcall, :z]]]],
  "* =f,g rescue b and c" =>
    [[:rescue, [:masgn, nil, [:splat], [:array, [:vcall, :f], [
    :vcall, :g]]], [:resbody, nil, [:and, [:vcall, :b], [:vcall,
     :c]]]]],

  "*a0 = (*a= c)" =>
    [[:masgn, nil, [:lasgn, :a0], [:array, [:masgn, nil, [:lasgn,
     :a], [:array, [:vcall, :c]]]]]],

  "*a0 = *(a= c)" =>
    [[:masgn, nil, [:lasgn, :a0], [:splat, [:lasgn, :a, [:vcall,
     :c]]]]],

  "*a0 = *a=b" =>
    [[:masgn, nil, [:lasgn, :a0], [:splat, [:lasgn, :a, [:vcall,
     :b]]]]],

  "*a0 = *a=c" =>
    [[:masgn, nil, [:lasgn, :a0], [:splat, [:lasgn, :a, [:vcall,
     :c]]]]],

  "*a0= begin a end rescue b0" =>
    [[:rescue, [:masgn, nil, [:lasgn, :a0], [:array, [:vcall, :a]
    ]], [:resbody, nil, [:vcall, :b0]]]],

  "*a=*b" => [[:masgn, nil, [:lasgn, :a], [:splat, [:vcall, :b]]]],
  "*a=b" => [[:masgn, nil, [:lasgn, :a], [:array, [:vcall, :b]]]],
  "*a=b rescue c" =>
    [[:rescue, [:masgn, nil, [:lasgn, :a], [:array, [:vcall, :b]
    ]], [:resbody, nil, [:vcall, :c]]]],

  "*a[*b]=c" =>
    [[:masgn, nil, [:attrasgn, [:vcall, :a], :[]=, [:splat, [
    :vcall, :b]]], [:array, [:vcall, :c]]]],

  "*a[b=>c,*d]=e" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\n*a[b=>c,*d]=e\n         ^"),
  "*a[b=>c]=d" =>
    [[:masgn, nil, [:attrasgn, [:vcall, :a], :[]=, [:array, [
    :hash, [:vcall, :b], [:vcall, :c]]]], [:array, [:vcall, :d]]
    ]],

  "*a[y,z,*b]=c" =>
    [[:masgn, nil, [:attrasgn, [:vcall, :a], :[]=, [:argscat, [
    :array, [:vcall, :y], [:vcall, :z]], [:vcall, :b]]], [:array, [
    :vcall, :c]]]],

  "*a[z,*b]=c" =>
    [[:masgn, nil, [:attrasgn, [:vcall, :a], :[]=, [:argscat, [
    :array, [:vcall, :z]], [:vcall, :b]]], [:array, [:vcall, :c]
    ]]],

  "*a[z]=c rescue b" =>
    [[:rescue, [:masgn, nil, [:attrasgn, [:vcall, :a], :[]=, [
    :array, [:vcall, :z]]], [:array, [:vcall, :c]]], [:resbody,
     nil, [:vcall, :b]]]],

  "*d=e,*f" =>
    [[:masgn, nil, [:lasgn, :d], [:argscat, [:array, [:vcall, :e]
    ], [:vcall, :f]]]],

  "*d=e,f" =>
    [[:masgn, nil, [:lasgn, :d], [:array, [:vcall, :e], [:vcall,
     :f]]]],

  "*z[begin; a; rescue b; end]=*c" =>
    [[:masgn, nil, [:attrasgn, [:vcall, :z], :[]=, [:array, [
    :rescue, [:vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]
    ], [:splat, [:vcall, :c]]]],

  "+ %r{r\#{1}}" =>
    [[:call, [:dregx, "r", [:evstr, [:lit, 1]]], :+@]],

  "+ %r{r}" => [[:lit, /r/]],
  "+ %s{s}" => [[:lit, :s]],
  "+ /r\#{1}/" =>
    [[:call, [:dregx, "r", [:evstr, [:lit, 1]]], :+@]],

  "+ /r/" => [[:lit, /r/]],
  "+ 1" => [[:lit, 1]],
  "+ :\"s\"" => [[:lit, :s]],
  "+ :\"s\#{1}\"" =>
    [[:call, [:dsym, "s", [:evstr, [:lit, 1]]], :+@]],

  "+ :'s'" => [[:lit, :s]],
  "+ :s" => [[:lit, :s]],
  "+ ?c" => [[:lit, 99]],
  "+ ?c;p" =>
    [[:vcall, :p],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "+\"r\";p" =>
    [[:block, [:call, [:str, "r"], :+@], [:vcall, :p]],
    {:warnings=>["(string):1: warning: useless use of +@ in void context"]}],

  "+%r{r\#{1}}" =>
    [[:call, [:dregx, "r", [:evstr, [:lit, 1]]], :+@]],

  "+%r{r}" => [[:lit, /r/]],
  "+%s{s}" => [[:lit, :s]],
  "+%s{s};p" =>
    [[:vcall, :p],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "+++++++++++-1" => [[:lit, -1]],
  "+++++++++++-1;p" =>
    [[:vcall, :p],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "+++++++++++1" => [[:lit, 1]],
  "+++++++++++1;p" =>
    [[:vcall, :p],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "++++++-+++++1" =>
    [[:call, [:call, [:call, [:call, [:call, [:call, [:call, [
    :lit, 1], :-@], :+@], :+@], :+@], :+@], :+@], :+@]],

  "+-+-+++++---+++-++-0" =>
    [[:call, [:call, [:call, [:call, [:call, [:call, [:call, [
    :call, [:call, [:call, [:call, [:call, [:call, [:call, [
    :call, [:call, [:lit, 0], :-@], :+@], :+@], :+@], :-@], :-@],
     :-@], :+@], :+@], :+@], :+@], :+@], :-@], :+@], :-@], :+@]],

  "+/r\#{1}/" =>
    [[:call, [:dregx, "r", [:evstr, [:lit, 1]]], :+@]],

  "+/r/" => [[:lit, /r/]],
  "+/r/;p" =>
    [[:vcall, :p],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "+/reg\#{exp}/" =>
    [[:call, [:dregx, "reg", [:evstr, [:vcall, :exp]]], :+@]],

  "+1" => [[:lit, 1]],
  "+:\"s\"" => [[:lit, :s]],
  "+:\"s\#{1}\"" =>
    [[:call, [:dsym, "s", [:evstr, [:lit, 1]]], :+@]],

  "+:'s'" => [[:lit, :s]],
  "+:'s';p" =>
    [[:vcall, :p],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "+:s" => [[:lit, :s]],
  "+:s;p" =>
    [[:vcall, :p],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "+?c" => [[:lit, 99]],
  "+?c;p" =>
    [[:vcall, :p],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "+a**2" =>
    [[:call, [:call, [:vcall, :a], :+@], :**, [:array, [:lit, 2]
    ]]],

  "+a[2]" =>
    [[:call, [:call, [:vcall, :a], :[], [:array, [:lit, 2]]], :+@]
    ],

  "+begin; a; rescue b; end" =>
    [[:call, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], :+@]],

  "- 1" => [[:call, [:lit, 1], :-@]],
  "-+++++++++++1" => [[:call, [:lit, 1], :-@]],
  "--1" => [[:call, [:lit, -1], :-@]],
  "--a" => [[:call, [:call, [:vcall, :a], :-@], :-@]],
  "-0**31" =>
    [[:call, [:call, [:lit, 0], :**, [:array, [:lit, 31]]], :-@]
    ],

  "-0.0**31" =>
    [[:call, [:call, [:lit, 0.0], :**, [:array, [:lit, 31]]], :-@]
    ],

  "\
-0.0
__END__
**31" =>
    [[:lit, -0.0]],

  "-0.0e0**31" =>
    [[:call, [:call, [:lit, 0.0], :**, [:array, [:lit, 31]]], :-@]
    ],

  "-0e0**31" =>
    [[:call, [:call, [:lit, 0.0], :**, [:array, [:lit, 31]]], :-@]
    ],

  "-2**3.1" =>
    [[:call, [:call, [:lit, 2], :**, [:array, [:lit, 3.1]]], :-@]
    ],

  "-2**31" =>
    [[:call, [:call, [:lit, 2], :**, [:array, [:lit, 31]]], :-@]
    ],

  "-2.7**31" =>
    [[:call, [:call, [:lit, 2.7], :**, [:array, [:lit, 31]]], :-@]
    ],

  "-2.7e8**31" =>
    [[:call, [:call, [:lit, 270000000.0], :**, [:array, [:lit, 31]
    ]], :-@]],

  "-2.c" => [[:call, [:lit, -2], :c]],
  "-2::c" => [[:call, [:lit, -2], :c]],
  "-2[3]" => [[:call, [:lit, -2], :[], [:array, [:lit, 3]]]],
  "-2[3]=4" =>
    [[:attrasgn, [:lit, -2], :[]=, [:array, [:lit, 3], [:lit, 4]
    ]]],

  "-2e7**31" =>
    [[:call, [:call, [:lit, 20000000.0], :**, [:array, [:lit, 31]
    ]], :-@]],

  "-a**2" =>
    [[:call, [:call, [:vcall, :a], :**, [:array, [:lit, 2]]], :-@]
    ],

  "-begin; a; rescue b; end" =>
    [[:call, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], :-@]],

  "/\#{}/" => [[:dregx, "", [:evstr]]],
  "//!~a" => [[:not, [:match2, [:lit, //], [:vcall, :a]]]],
  "//nonsense" => [[:lit, //n]],
  "//xenon" => [[:lit, //x]],
  "/[}]/" => [[:lit, /[}]/]],
  "/\\ /x" => [[:lit, /\ /x]],
  "/\\@/" => [[:lit, /\@/]],
  "/\\y/" => [[:lit, /y/]],
  "/\\}/" => [[:lit, /\}/]],
  "/a b \#{c}/" => [[:dregx, "a b ", [:evstr, [:vcall, :c]]]],
  "/a\#{\"b\"}c\#{__FILE__}e\#{\"f\"}/" =>
    [[:lit, /abc(string)ef/]],

  "/a\#{\"b\"}c\#{__FILE__}e\#{\"f\"}/o" =>
    [[:lit, /abc(string)ef/]],

  "/a\#{b}/i" => [[:dregx, "a", [:evstr, [:vcall, :b]], 1]],
  "/a\#{b}/imox" =>
    [[:dregx_once, "a", [:evstr, [:vcall, :b]], 7]],

  "/a\#{b}/m" => [[:dregx, "a", [:evstr, [:vcall, :b]], 4]],
  "/a\#{b}/o" => [[:dregx_once, "a", [:evstr, [:vcall, :b]]]],
  "/a\#{b}/x" => [[:dregx, "a", [:evstr, [:vcall, :b]], 2]],
  "/a\#{b}c/i" =>
    [[:dregx, "a", [:evstr, [:vcall, :b]], [:str, "c"], 1]],

  "/a\#{b}c/imox" =>
    [[:dregx_once, "a", [:evstr, [:vcall, :b]], [:str, "c"], 7]],

  "/a\#{b}c/m" =>
    [[:dregx, "a", [:evstr, [:vcall, :b]], [:str, "c"], 4]],

  "/a\#{b}c/o" =>
    [[:dregx_once, "a", [:evstr, [:vcall, :b]], [:str, "c"]]],

  "/a\#{b}c/x" =>
    [[:dregx, "a", [:evstr, [:vcall, :b]], [:str, "c"], 2]],

  "/a...b/" => [[:lit, /a...b/]],
  "/a..b/" => [[:lit, /a..b/]],
  "/a..b/ and c" => [[:and, [:lit, /a..b/], [:vcall, :c]]],
  "/a..b/ or c" => [[:or, [:lit, /a..b/], [:vcall, :c]]],
  "/a/" => [[:lit, /a/]],
  "/a/; b" =>
    [[:vcall, :b],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "/a/i" => [[:lit, /a/i]],
  "/a/imox" => [[:lit, /a/mix]],
  "/a/m" => [[:lit, /a/m]],
  "/a/o" => [[:lit, /a/]],
  "/a/x" => [[:lit, /a/x]],
  "/vt100/ =~ line" =>
    [[:match2, [:lit, /vt100/], [:vcall, :line]]],

  "/vt100/ =~ line and f" =>
    [[:and, [:match2, [:lit, /vt100/], [:vcall, :line]], [:vcall,
     :f]]],

  "0" => [[:lit, 0]],
  "0...0;a" =>
    [[:block, [:lit, 0...0], [:vcall, :a]],
    {:warnings=>["(string):1: warning: useless use of ... in void context"]}],

  "0..0;a" =>
    [[:block, [:lit, 0..0], [:vcall, :a]],
    {:warnings=>["(string):1: warning: useless use of .. in void context"]}],

  "0.0" => [[:lit, 0.0]],
  "0.0;a" =>
    [[:vcall, :a],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "0.113725" => [[:lit, 0.113725]],
  "0.5" => [[:lit, 0.5]],
  "0.777777" => [[:lit, 0.777777]],
  "0234526546" => [[:lit, 41069926]],
  "0b1010001001011111" => [[:lit, 41567]],
  "0d57896675" => [[:lit, 57896675]],
  "0e0" => [[:lit, 0.0]],
  "0o234526546" => [[:lit, 41069926]],
  "0x1134234aefeb" => [[:lit, 18915628085227]],
  "1" => [[:lit, 1]],
  "1+1" => [[:call, [:lit, 1], :+, [:array, [:lit, 1]]]],
  "1.+?y" => [[:call, [:lit, 1], :+, [:array, [:lit, 121]]]],
  "1..2" => [[:lit, 1..2]],
  "1;a" =>
    [[:vcall, :a],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "2**3[4]" =>
    [[:call, [:lit, 2], :**, [:array, [:call, [:lit, 3], :[], [
    :array, [:lit, 4]]]]]],

  "2+3*4" =>
    [[:call, [:lit, 2], :+, [:array, [:call, [:lit, 3], :*, [
    :array, [:lit, 4]]]]]],

  "\
2
__END__
foo bar baz\

" =>
    [[:lit, 2]],

  "6._ = 7" => [[:attrasgn, [:lit, 6], :_=, [:array, [:lit, 7]]]],
  "6._ =7" => [[:attrasgn, [:lit, 6], :_=, [:array, [:lit, 7]]]],
  "6._= 7" => [[:attrasgn, [:lit, 6], :_=, [:array, [:lit, 7]]]],
  "6._=7" => [[:attrasgn, [:lit, 6], :_=, [:array, [:lit, 7]]]],
  "6._?" => [[:call, [:lit, 6], :_?]],
  "\
:\"
\"" =>
    [[:lit, :"\n"]],

  ":\"\\\\y\"" => [[:lit, :"\\y"]],
  ":\"\\y\"" => [[:lit, :y]],
  ":\"a\#{b}\"" => [[:dsym, "a", [:evstr, [:vcall, :b]]]],
  ":\"s\#{1}\"" => [[:dsym, "s", [:evstr, [:lit, 1]]]],
  ":\"y!\"" => [[:lit, :y!]],
  ":\"y\"" => [[:lit, :y]],
  ":\"y=\"" => [[:lit, :y=]],
  ":\"y?\"" => [[:lit, :y?]],
  ":\"y\\\"\"" => [[:lit, :"y\""]],
  ":'\\\\'" => [[:lit, :"\\"]],
  "::A" => [[:colon3, :A]],
  "::B" => [[:colon3, :B]],
  ":a" => [[:lit, :a]],
  ":foo" => [[:lit, :foo]],
  ":~@" => [[:lit, :~]],
  ";1;2;" =>
    [[:lit, 2],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  ";a;2;" => [[:block, [:vcall, :a], [:lit, 2]]],
  "\
<<'E'
1
E\

" =>
    [[:str, "1\n"]],

  "\
<<-EOS<<__LINE__
EOS" =>
    [[:call, [:str, ""], :<<, [:array, [:lit, 1]]]],

  "\
<<ENDModelCode
   
class \#{@modelClassName} 
end
\nENDModelCode
\n" =>
    [[:dstr, "   \nclass ", [:evstr, [:ivar, :@modelClassName]],
     [:str, " \nend\n\n"]]],

  "\
<<E
E" =>
    [[:str, ""]],

  "\
<<HEADER
class \#{filename.capitalize}TestCase
        def set_up
                @\#{filename.downcase}s
                @\#{filename.downcase}s
        end
end
HEADER
\n" =>
    [[:dstr, "class ", [:evstr, [:call, [:vcall, :filename],
     :capitalize]], [:str, "TestCase\n        def set_up\n                @"], [
    :evstr, [:call, [:vcall, :filename], :downcase]], [
    :str, "s\n                @"], [:evstr, [:call, [
    :vcall, :filename], :downcase]], [:str, "s\n        end\nend\n"]
    ]],

  "\
<<SRC+<<SRC
\#{headers}
SRC
\#{headers}
SRC\

" =>
    [[:call, [:dstr, "", [:evstr, [:vcall, :headers]], [:str, "\n"]
    ], :+, [:array, [:dstr, "", [:evstr, [:vcall, :headers]], [
    :str, "\n"]]]]],

  "\
<<Z  
\n\#@m
\nZ
\n" =>
    [[:dstr, "\n", [:evstr, [:ivar, :@m]], [:str, "\n\n"]]],

  "\
<<a+<<b+
\#{c}
345234
a
\#{d}
234523452
b
\"sdfsdf\"\

" =>
    [[:call, [:call, [:dstr, "", [:evstr, [:vcall, :c]], [:str,
     "\n345234\n"]], :+, [:array, [:dstr, "", [:evstr, [:vcall,
     :d]], [:str, "\n234523452\n"]]]], :+, [:array, [:str, "sdfsdf"]
    ]]],

  "\
<<a+<<b+\\
\#{c}
345234
a
\#{d}
234523452
b
\"sdfsdf\"\

" =>
    [[:call, [:call, [:dstr, "", [:evstr, [:vcall, :c]], [:str,
     "\n345234\n"]], :+, [:array, [:dstr, "", [:evstr, [:vcall,
     :d]], [:str, "\n234523452\n"]]]], :+, [:array, [:str, "sdfsdf"]
    ]]],

  "\
<<a+<<b
\#{c}
345234
a
\#{d}
234523452
b\

" =>
    [[:call, [:dstr, "", [:evstr, [:vcall, :c]], [:str, "\n345234\n"]
    ], :+, [:array, [:dstr, "", [:evstr, [:vcall, :d]], [:str,
     "\n234523452\n"]]]]],

  "\
<<p
p
\n" =>
    [[:str, ""]],

  "\
<<x.
 1111
x
a0 rescue b0()\

" =>
    [[:rescue, [:call, [:str, " 1111\n"], :a0], [:resbody, nil, [
    :fcall, :b0]]]],

  "\
<<x.
 1111
x
delete()\

" =>
    [[:call, [:str, " 1111\n"], :delete]],

  "\
<<x::
 1111
x
delete()\

" =>
    [[:call, [:str, " 1111\n"], :delete]],

  "\
=begin =end
=end" =>
    [],

  "\
=begin a
b
=end c" =>
    [],

  "\
=begin
=end" =>
    [],

  "\
=begin
=end\

" =>
    [],

  "\
=begin
foo
=end\

" =>
    [],

  "?\\C-?" => [[:lit, 127]],
  "?\\C-\\?" => [[:lit, 31]],
  "\
?\\
__END__
-?" =>
    [[:lit, 10]],

  "@@a" => [[:cvar, :@@a]],
  "@@a+=b" =>
    [[:cvdecl, :@@a, [:call, [:cvar, :@@a], :+, [:array, [:vcall,
     :b]]]]],

  "@@a=b" => [[:cvdecl, :@@a, [:vcall, :b]]],
  "@@a||=b" =>
    [[:op_asgn_or, [:cvar, :@@a], [:cvdecl, :@@a, [:vcall, :b]]]
    ],

  "@@foo::Bar" => [[:colon2, [:cvar, :@@foo], :Bar]],
  "@a" => [[:ivar, :@a]],
  "@a+=b" =>
    [[:iasgn, :@a, [:call, [:ivar, :@a], :+, [:array, [:vcall, :b]
    ]]]],

  "@a=b" => [[:iasgn, :@a, [:vcall, :b]]],
  "@a||=b" =>
    [[:op_asgn_or, [:ivar, :@a], [:iasgn, :@a, [:vcall, :b]]]],

  "@data.[]=(*args)" =>
    [[:call, [:ivar, :@data], :[]=, [:splat, [:vcall, :args]]]],

  "@foo::Bar" => [[:colon2, [:ivar, :@foo], :Bar]],
  "A :: B" => [[:fcall, :A, [:array, [:colon3, :B]]]],
  "A ::B" => [[:fcall, :A, [:array, [:colon3, :B]]]],
  "A+=b" =>
    [[:cdecl, :A, [:call, [:const, :A], :+, [:array, [:vcall, :b]
    ]]]],

  "A.b" => [[:call, [:const, :A], :b]],
  "A.b do end" => [[:iter, [:call, [:const, :A], :b], nil]],
  "A:: B" => [[:colon2, [:const, :A], :B]],
  "A::B" => [[:colon2, [:const, :A], :B]],
  "A::B::C" => [[:colon2, [:colon2, [:const, :A], :B], :C]],
  "\
A::
b\

" =>
    [[:call, [:const, :A], :b]],

  "A=b" => [[:cdecl, :A, [:vcall, :b]]],
  "A||=b" =>
    [[:op_asgn_or, [:const, :A], [:cdecl, :A, [:vcall, :b]]]],

  "B::c=d" =>
    [[:attrasgn, [:const, :B], :c=, [:array, [:vcall, :d]]]],

  "\
BEGIN {
  puts \"b1\"
  local_begin1 = \"local_begin1\"
  $global_begin1 = \"global_begin1\"
  ConstBegin1 = \"ConstBegin1\"
}
BEGIN {
  puts \"b2\"
  BEGIN {
    puts \"b2-1\"
    local_begin2=33
  }
}
# for scope check
raise if defined?(local_begin1)
raise if defined?(local_begin2)
raise unless defined?($global_begin1)
raise unless defined?(::ConstBegin1)
local_for_end2 = \"e2\"
$global_for_end1 = \"e1\"" =>
    [[:block, [:if, [:defined, [:vcall, :local_begin1]], [:vcall,
     :raise], nil], [:if, [:defined, [:vcall, :local_begin2]], [
    :vcall, :raise], nil], [:if, [:defined, [:gvar, :$global_begin1]
    ], nil, [:vcall, :raise]], [:if, [:defined, [:colon3, :ConstBegin1]
    ], nil, [:vcall, :raise]], [:lasgn, :local_for_end2, [:str,
     "e2"]], [:gasgn, :$global_for_end1, [:str, "e1"]]]],

  "BEGIN {a}" => [],
  "BEGIN {a};b" => [[:vcall, :b]],
  "BEGIN {}" => [],
  "BEGIN {};a" => [[:vcall, :a]],
  "BEGIN{ END{a=1};a }" => [],
  "\
BEGIN{44}; \\
__END__" =>
    [],

  "\
BEGIN{44}; \\
__END__\

" =>
    [],

  "BEGIN{a=1};a" => [[:vcall, :a]],
  "DIR do end" => [[:iter, [:fcall, :DIR], nil]],
  "Dir.chdir DIR do end" =>
    [[:iter, [:call, [:const, :Dir], :chdir, [:array, [:const,
     :DIR]]], nil]],

  "Dir.chdir DIR { }" =>
    [[:call, [:const, :Dir], :chdir, [:array, [:iter, [:fcall,
     :DIR], nil]]]],

  "E if defined? :E" =>
    [[:if, [:defined, [:lit, :E]], [:const, :E], nil]],

  "\
END {
  p \"bye-bye\"
}\

" =>
    [[:iter, [:postexe], nil, [:fcall, :p, [:array, [:str, "bye-bye"]
    ]]]],

  "\
END {
  puts \"b1\"
  local_begin1 = \"local_begin1\"
  $global_begin1 = \"global_begin1\"
  ConstBegin1 = \"ConstBegin1\"
}
END {
  puts \"b2\"
  END {
    puts \"b2-1\"
    local_begin2=33
  }
}
# for scope check
raise if defined?(local_begin1)
raise if defined?(local_begin2)
raise unless defined?($global_begin1)
raise unless defined?(::ConstBegin1)
local_for_end2 = \"e2\"
$global_for_end1 = \"e1\"" =>
    [[:block, [:iter, [:postexe], nil, [:block, [:fcall, :puts, [
    :array, [:str, "b1"]]], [:lasgn, :local_begin1, [
    :str, "local_begin1"]], [:gasgn, :$global_begin1, [
    :str, "global_begin1"]], [:cdecl, :ConstBegin1, [
    :str, "ConstBegin1"]]]], [:iter, [:postexe], nil, [:block, [
    :fcall, :puts, [:array, [:str, "b2"]]], [:iter, [:postexe],
     nil, [:block, [:fcall, :puts, [:array, [:str, "b2-1"]]], [
    :lasgn, :local_begin2, [:lit, 33]]]]]], [:if, [:defined, [
    :lvar, :local_begin1]], [:vcall, :raise], nil], [:if, [
    :defined, [:lvar, :local_begin2]], [:vcall, :raise], nil], [
    :if, [:defined, [:gvar, :$global_begin1]], nil, [:vcall, :raise]], [
    :if, [:defined, [:colon3, :ConstBegin1]], nil, [:vcall, :raise]
    ], [:lasgn, :local_for_end2, [:str, "e2"]], [:gasgn, :$global_for_end1, [
    :str, "e1"]]]],

  "END {a}" => [[:iter, [:postexe], nil, [:vcall, :a]]],
  "END {a};b" =>
    [[:block, [:iter, [:postexe], nil, [:vcall, :a]], [:vcall, :b]
    ]],

  "END {}" => [[:iter, [:postexe], nil]],
  "END {};a" => [[:block, [:iter, [:postexe], nil], [:vcall, :a]]],
  "END{ BEGIN{a=1};a }" =>
    [[:iter, [:postexe], nil, [:vcall, :a]]],

  "END{a=1};a" =>
    [[:block, [:iter, [:postexe], nil, [:lasgn, :a, [:lit, 1]]],
     [:lvar, :a]]],

  "FALSE" => [[:const, :FALSE]],
  "File.open() {|f|  ;  }" =>
    [[:iter, [:call, [:const, :File], :open], [:dasgn_curr, :f]]
    ],

  "\
File.open(ARGV.first){|f|
  deficit=0
  while buf=f.read(1024+deficit)
    #trim possibly incomplete sequences from end
    if /\\\\[0-7]{0,2}\\Z/===buf
      deficit=$&.size
      buf[-deficit..-1]=''
      f.pos-=deficit
    else 
      deficit=0
    end
\n    $stdout.write buf.gsub(/\\\\([0-7]{1,3})/){
      $1.oct.&(0xFF).chr 
    }
  end
}\

" =>
    [[:iter, [:call, [:const, :File], :open, [:array, [:call, [
    :const, :ARGV], :first]]], [:dasgn_curr, :f], [:block, [
    :dasgn_curr, :deficit, [:lit, 0]], [:while, [:dasgn_curr, :buf, [
    :call, [:dvar, :f], :read, [:array, [:call, [:lit, 1024], :+, [
    :array, [:dvar, :deficit]]]]]], [:block, [:if, [:call, [:lit,
     /\\[0-7]{0,2}\Z/], :===, [:array, [:dvar, :buf]]], [:block,
     [:dasgn_curr, :deficit, [:call, [:back_ref, :&], :size]], [
    :attrasgn, [:dvar, :buf], :[]=, [:array, [:dot2, [:call, [
    :dvar, :deficit], :-@], [:lit, -1]], [:str, ""]]], [:op_asgn2, [
    :dvar, :f], :pos=, :-, [:dvar, :deficit]]], [:dasgn_curr, :deficit, [
    :lit, 0]]], [:call, [:gvar, :$stdout], :write, [:array, [
    :iter, [:call, [:dvar, :buf], :gsub, [:array, [
    :lit, /\\([0-7]{1,3})/]]], nil, [:call, [:call, [:call, [
    :nth_ref, 1], :oct], :&, [:array, [:lit, 255]]], :chr]]]]],
     true]]]],

  "Foop.bar 1,2" =>
    [[:call, [:const, :Foop], :bar, [:array, [:lit, 1], [:lit, 2]
    ]]],

  "Foop::bar 3,4" =>
    [[:call, [:const, :Foop], :bar, [:array, [:lit, 3], [:lit, 4]
    ]]],

  "Hasnew{[]}" => [[:iter, [:fcall, :Hasnew], nil, [:zarray]]],
  "JAVASCRIPTS.each { |script| @xml.script :type => 'text/javascript',\
 :src => \"/javascripts/\#{script}.js\" do end }" =>
    [[:iter, [:call, [:const, :JAVASCRIPTS], :each], [:dasgn_curr,
     :script], [:iter, [:call, [:ivar, :@xml], :script, [:array,
     [:hash, [:lit, :type], [:str, "text/javascript"], [:lit, :src], [
    :dstr, "/javascripts/", [:evstr, [:dvar, :script]], [:str,
     ".js"]]]]], nil]]],

  "NIL" => [[:const, :NIL]],
  "P ::Class" => [[:fcall, :P, [:array, [:colon3, :Class]]]],
  "P? :p8" => [[:fcall, :P?, [:array, [:lit, :p8]]]],
  "P? ?1" => [[:fcall, :P?, [:array, [:lit, 49]]]],
  "P?:p8" => [[:fcall, :P?, [:array, [:lit, :p8]]]],
  "P??1" => [[:fcall, :P?, [:array, [:lit, 49]]]],
  "Proc{|a,&b| c}" => SyntaxError.new("(string):1: syntax error, unexpected tAMPER, expecting '|'\nProc{|a,&b| c}\n         ^\n(string):1: syntax error, unexpected '}', expecting $end\nProc{|a,&b| c}\n              ^"),
  "Proc{|a,&b|}" => SyntaxError.new("(string):1: syntax error, unexpected tAMPER, expecting '|'\nProc{|a,&b|}\n         ^"),
  "RuntimeError.new(\"foo\")" =>
    [[:call, [:const, :RuntimeError], :new, [:array, [:str, "foo"]
    ]]],

  "RuntimeError.new()" => [[:call, [:const, :RuntimeError], :new]],
  "Sw.===(anything).should == true" =>
    [[:call, [:call, [:call, [:const, :Sw], :===, [:array, [
    :vcall, :anything]]], :should], :==, [:array, [:true]]]],

  "TRUE" => [[:const, :TRUE]],
  "UnOpNode===arg and /^$/===arg" =>
    [[:and, [:call, [:const, :UnOpNode], :===, [:array, [:vcall,
     :arg]]], [:call, [:lit, /^$/], :===, [:array, [:vcall, :arg]
    ]]]],

  "X.I = 1" =>
    [[:attrasgn, [:const, :X], :I=, [:array, [:lit, 1]]]],

  "X::I = 1" => [[:cdecl, [:colon2, [:const, :X], :I], [:lit, 1]]],
  "[\"2266\", \"\#{22}\" \"\#{44}\" \"55\" \"\#{66}\"]" =>
    [[:array, [:str, "2266"], [:dstr, "", [:evstr, [:lit, 22]], [
    :evstr, [:lit, 44]], [:str, "55"], [:evstr, [:lit, 66]]]]],

  "[*a]" => [[:splat, [:vcall, :a]]],
  "[*b=c]" => [[:splat, [:lasgn, :b, [:vcall, :c]]]],
  "[*b]" => [[:splat, [:vcall, :b]]],
  "[3,4]" => [[:array, [:lit, 3], [:lit, 4]]],
  "[3]" => [[:array, [:lit, 3]]],
  "[]" => [[:zarray]],
  "[a,*b=c]" =>
    [[:argscat, [:array, [:vcall, :a]], [:lasgn, :b, [:vcall, :c]
    ]]],

  "[a,*b]" => [[:argscat, [:array, [:vcall, :a]], [:vcall, :b]]],
  "[a,b,*c]" =>
    [[:argscat, [:array, [:vcall, :a], [:vcall, :b]], [:vcall, :c]
    ]],

  "[a,b]" => [[:array, [:vcall, :a], [:vcall, :b]]],
  "[a0 = a rescue b,c]" =>
    [[:array, [:lasgn, :a0, [:rescue, [:vcall, :a], [:resbody, nil, [
    :vcall, :b]]]], [:vcall, :c]]],

  "[a=>b,c=>d,*e]" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\n[a=>b,c=>d,*e]\n            ^"),
  "[a=>b,c=>d]" =>
    [[:array, [:hash, [:vcall, :a], [:vcall, :b], [:vcall, :c], [
    :vcall, :d]]]],

  "[a=b=1,d=2]" =>
    [[:array, [:lasgn, :a, [:lasgn, :b, [:lit, 1]]], [:lasgn, :d, [
    :lit, 2]]]],

  "[a]" => [[:array, [:vcall, :a]]],
  "[b=>c,d=>e]" =>
    [[:array, [:hash, [:vcall, :b], [:vcall, :c], [:vcall, :d], [
    :vcall, :e]]]],

  "[b=>c]" => [[:array, [:hash, [:vcall, :b], [:vcall, :c]]]],
  "[begin; a; rescue b; end]" =>
    [[:array, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]]]],

  "[y,z,*b]" =>
    [[:argscat, [:array, [:vcall, :y], [:vcall, :z]], [:vcall, :b]
    ]],

  "[z,*b]" => [[:argscat, [:array, [:vcall, :z]], [:vcall, :b]]],
  "\
\te.hour %= 12;\t( downcase == 'p')
\te.hour %= 12;\tbegin downcase == 'p'; end
\te.hour %= 12;\tif downcase == 'p'; end
\te.hour %= 12;\t(downcase)
\te.hour %= 12;\tif downcase; end
\n" =>
    [[:block, [:op_asgn2, [:vcall, :e], :hour=, :%, [:lit, 12]],
     [:call, [:vcall, :downcase], :==, [:array, [:str, "p"]]], [
    :op_asgn2, [:vcall, :e], :hour=, :%, [:lit, 12]], [:call, [
    :vcall, :downcase], :==, [:array, [:str, "p"]]], [:op_asgn2,
     [:vcall, :e], :hour=, :%, [:lit, 12]], [:if, [:call, [
    :vcall, :downcase], :==, [:array, [:str, "p"]]], nil, nil], [
    :op_asgn2, [:vcall, :e], :hour=, :%, [:lit, 12]], [:vcall,
     :downcase], [:op_asgn2, [:vcall, :e], :hour=, :%, [:lit, 12]
    ], [:if, [:vcall, :downcase], nil, nil]],
    {:warnings=>["(string):1: warning: useless use of == in void context", "(string):2: warning: useless use of == in void context"]}],

  "
<<here
\#{<<there
over there, over there, when its over over there.
there
}
here\

" =>
    [[:str, "over there, over there, when its over over there.\n\n"]
    ],

  "
A::
B\

" =>
    [[:colon2, [:const, :A], :B]],

  "
[a \\
,b]\

" =>
    [[:array, [:vcall, :a], [:vcall, :b]]],

  "
__END__
\n" =>
    [],

  "
class AA; class BB; class CC
FFOO=1
end end end
p AA::BB::CC::FFOO\

" =>
    [[:block, [:class, :AA, nil, [:scope, [:class, :BB, nil, [
    :scope, [:class, :CC, nil, [:scope, [:cdecl, :FFOO, [:lit, 1]
    ]]]]]]], [:fcall, :p, [:array, [:colon2, [:colon2, [:colon2,
     [:const, :AA], :BB], :CC], :FFOO]]]]],

  "
class Foop
  def Foop::baz a,b
    p :baz,a,b
  end
end
Foop.baz 5,6
Foop::baz 7,8\

" =>
    [[:block, [:class, :Foop, nil, [:scope, [:defs, [
    :const, :Foop], :baz, [:scope, [:block, [:args, :a, :b], [
    :fcall, :p, [:array, [:lit, :baz], [:lvar, :a], [:lvar, :b]]
    ]]]]]], [:call, [:const, :Foop], :baz, [:array, [:lit, 5], [
    :lit, 6]]], [:call, [:const, :Foop], :baz, [:array, [:lit, 7], [
    :lit, 8]]]]],

  "
def add(*args)
   self.<<(*args)
end\

" =>
    [[:defn, :add, [:scope, [:block, [:args, :"*args"], [:call, [
    :self], :<<, [:splat, [:lvar, :args]]]]]]],

  "
def printem1 a,b,c
   p(a +77)
   p(b +77)
   p(c +77)
end\

" =>
    [[:defn, :printem1, [:scope, [:block, [:args, :a, :b, :c], [
    :fcall, :p, [:array, [:call, [:lvar, :a], :+, [:array, [:lit,
     77]]]]], [:fcall, :p, [:array, [:call, [:lvar, :b], :+, [
    :array, [:lit, 77]]]]], [:fcall, :p, [:array, [:call, [:lvar,
     :c], :+, [:array, [:lit, 77]]]]]]]]],

  "
def ssssss &block
end\

" =>
    [[:defn, :ssssss, [:scope, [:block, [:args], [:block_arg, :block], [
    :nil]]]]],

  "
defined? <<A
sdsdfsdfs
A\

" =>
    [[:defined, [:str, "sdsdfsdfs\n"]]],

  "
module Y19  #limit lvar scope
  a,b,c,(d,e)=1,2,3,[4,5]
  p a %(4)
  p c %(4)
  p d %(4)
  p e %(4)
  a=[1,2,3,4,5]
  def self.g(x=nil); 3 end
  def a.g=(x) p x end
  g = 5
  self.g = 55
  class<<a
    def bb=(x) p :bb=, x end
  end
  A=a
  class<<self
  def aa; :aa end
  def bb(arg=nil); p :bb; A end
  alias bbb bb
  def m; self end
  def n; self end
  def +(other) self end
  def kk; nil end
  def kk=(foo); nil end
  end
  proc{|a[4]|}.call 6
  proc{|a[b]|}.call 7
  proc{|a.bb| bb %(9) }.call 9
  proc{|a[f]| f %(9)  }.call 8
  proc{|bb(aa).bb| aa %(10) }.call 10
  proc{|bbb(aa).bb| bbb %(11) }.call 11
  proc{|t,u,(v,w,x),(y,),z|
    t %(12)
    u %(12)
    v %(12)
    w %(12)
    x %(12)
    y %(12)
    z %(12)
  }.call(1,2,[3,4,5],[6],7)
  proc{|(m).kk,(m+n).kk|
    m %(13)
    n %(13)
    kk %(13)
  }.call(13,14)
  proc{|a.g| g %(9)}
  p a
end\

" =>
    [[:module, :Y19, [:scope, [:block, [:masgn, [:array, [:lasgn,
     :a], [:lasgn, :b], [:lasgn, :c], [:masgn, [:array, [:lasgn,
     :d], [:lasgn, :e]], nil, nil]], nil, [:array, [:lit, 1], [
    :lit, 2], [:lit, 3], [:array, [:lit, 4], [:lit, 5]]]], [
    :fcall, :p, [:array, [:call, [:lvar, :a], :%, [:array, [:lit,
     4]]]]], [:fcall, :p, [:array, [:call, [:lvar, :c], :%, [
    :array, [:lit, 4]]]]], [:fcall, :p, [:array, [:call, [:lvar,
     :d], :%, [:array, [:lit, 4]]]]], [:fcall, :p, [:array, [
    :call, [:lvar, :e], :%, [:array, [:lit, 4]]]]], [:lasgn, :a,
     [:array, [:lit, 1], [:lit, 2], [:lit, 3], [:lit, 4], [:lit,
     5]]], [:defs, [:self], :g, [:scope, [:block, [:args, :x, [
    :block, [:lasgn, :x, [:nil]]]], [:lit, 3]]]], [:defs, [:lvar,
     :a], :g=, [:scope, [:block, [:args, :x], [:fcall, :p, [
    :array, [:lvar, :x]]]]]], [:lasgn, :g, [:lit, 5]], [:attrasgn, [
    :self], :g=, [:array, [:lit, 55]]], [:sclass, [:lvar, :a], [
    :scope, [:defn, :bb=, [:scope, [:block, [:args, :x], [:fcall,
     :p, [:array, [:lit, :bb=], [:lvar, :x]]]]]]]], [:cdecl, :A,
     [:lvar, :a]], [:sclass, [:self], [:scope, [:block, [:defn,
     :aa, [:scope, [:block, [:args], [:lit, :aa]]]], [:defn, :bb, [
    :scope, [:block, [:args, :arg, [:block, [:lasgn, :arg, [:nil]
    ]]], [:fcall, :p, [:array, [:lit, :bb]]], [:const, :A]]]], [
    :alias, [:lit, :bbb], [:lit, :bb]], [:defn, :m, [:scope, [
    :block, [:args], [:self]]]], [:defn, :n, [:scope, [:block, [
    :args], [:self]]]], [:defn, :+, [:scope, [:block, [:args, :other], [
    :self]]]], [:defn, :kk, [:scope, [:block, [:args], [:nil]]]
    ], [:defn, :kk=, [:scope, [:block, [:args, :foo], [:nil]]]]]
    ]], [:call, [:iter, [:fcall, :proc], [:attrasgn, [:lvar, :a],
     :[]=, [:array, [:lit, 4]]]], :call, [:array, [:lit, 6]]], [
    :call, [:iter, [:fcall, :proc], [:attrasgn, [:lvar, :a], :[]=, [
    :array, [:lvar, :b]]]], :call, [:array, [:lit, 7]]], [:call,
     [:iter, [:fcall, :proc], [:attrasgn, [:lvar, :a], :bb=], [
    :fcall, :bb, [:array, [:str, "9"]]]], :call, [:array, [:lit,
     9]]], [:call, [:iter, [:fcall, :proc], [:attrasgn, [:lvar,
     :a], :[]=, [:array, [:vcall, :f]]], [:fcall, :f, [:array, [
    :str, "9"]]]], :call, [:array, [:lit, 8]]], [:call, [:iter, [
    :fcall, :proc], [:attrasgn, [:fcall, :bb, [:array, [:vcall,
     :aa]]], :bb=], [:fcall, :aa, [:array, [:str, "10"]]]], :call, [
    :array, [:lit, 10]]], [:call, [:iter, [:fcall, :proc], [
    :attrasgn, [:fcall, :bbb, [:array, [:vcall, :aa]]], :bb=], [
    :fcall, :bbb, [:array, [:str, "11"]]]], :call, [:array, [
    :lit, 11]]], [:call, [:iter, [:fcall, :proc], [:masgn, [
    :array, [:dasgn_curr, :t], [:dasgn_curr, :u], [:masgn, [
    :array, [:dasgn_curr, :v], [:dasgn_curr, :w], [:dasgn_curr,
     :x]], nil, nil], [:masgn, [:array, [:dasgn_curr, :y]], nil,
     nil], [:dasgn_curr, :z]], nil, nil], [:block, [:call, [
    :dvar, :t], :%, [:array, [:lit, 12]]], [:call, [:dvar, :u],
     :%, [:array, [:lit, 12]]], [:call, [:dvar, :v], :%, [:array, [
    :lit, 12]]], [:call, [:dvar, :w], :%, [:array, [:lit, 12]]],
     [:call, [:dvar, :x], :%, [:array, [:lit, 12]]], [:call, [
    :dvar, :y], :%, [:array, [:lit, 12]]], [:call, [:dvar, :z],
     :%, [:array, [:lit, 12]]]]], :call, [:array, [:lit, 1], [
    :lit, 2], [:array, [:lit, 3], [:lit, 4], [:lit, 5]], [:array, [
    :lit, 6]], [:lit, 7]]], [:call, [:iter, [:fcall, :proc], [
    :masgn, [:array, [:attrasgn, [:vcall, :m], :kk=], [:attrasgn, [
    :call, [:vcall, :m], :+, [:array, [:vcall, :n]]], :kk=]], nil,
     nil], [:block, [:fcall, :m, [:array, [:str, "13"]]], [
    :fcall, :n, [:array, [:str, "13"]]], [:fcall, :kk, [:array, [
    :str, "13"]]]]], :call, [:array, [:lit, 13], [:lit, 14]]], [
    :iter, [:fcall, :proc], [:attrasgn, [:lvar, :a], :g=], [
    :call, [:lvar, :g], :%, [:array, [:lit, 9]]]], [:fcall, :p, [
    :array, [:lvar, :a]]]]]],
    {:warnings=>["(string):34: warning: useless use of % in void context", "(string):35: warning: useless use of % in void context", "(string):36: warning: useless use of % in void context", "(string):37: warning: useless use of % in void context", "(string):38: warning: useless use of % in void context", "(string):39: warning: useless use of % in void context"]}],

  "
module
=begin
  foo
=end
A::
=begin
  bar
=end
B; end\

" =>
    [[:module, [:colon2, [:const, :A], :B], [:scope]]],

  "
module
=begin
=end
A; end\

" =>
    [[:module, :A, [:scope]]],

  "
p <<-BEGIN + <<-END
          def element_downcase(attributes = {})
        BEGIN
          end
        END\

" =>
    [[:fcall, :p, [:array, [:call, [
    :str, "          def element_downcase(attributes = {})\n"],
     :+, [:array, [:str, "          end\n"]]]]]],

  "
p <<END
dfgdfg
END\

" =>
    [[:fcall, :p, [:array, [:str, "dfgdfg\n"]]]],

  "
p <<here
where?
here\

" =>
    [[:fcall, :p, [:array, [:str, "where?\n"]]]],

  "
x, (*), z = [:x, :y, :z]
p x
p z
\nx, (*y), z = [:x, :y, :z]
p x
p y
p z
\np($/ = ' '; Array( \"i'm in your house\" ))
\nclass Foou
 public
 def [] x=-100,&y=nil; p x; 100 end
end
p Foou.new.[]?9      #value
p Foou.new.[] ?9     #value\

" =>
    SyntaxError.new("(string):11: syntax error, unexpected ';', expecting ')'\np($/ = ' '; Array( \"i'm in your house\" ))\n           ^\n(string):11: syntax error, unexpected ')', expecting $end"),

  "
x{
  val=%[13,17,22,\"hike\", ?\\s]
    if val.include? ?\\s
      p val.split.collect{|v| (v)}
    end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :val, [
    :str, "13,17,22,\"hike\", ? "]], [:if, [:call, [:dvar, :val],
     :include?, [:array, [:lit, 32]]], [:fcall, :p, [:array, [
    :iter, [:call, [:call, [:dvar, :val], :split], :collect], [
    :dasgn_curr, :v], [:dvar, :v]]]], nil]]]],

  "
\n__END__
\n
\n
\n" =>
    [],

  "__END__" => [],
  "__END__  #with a comment" => [[:vcall, :__END__]],
  "\
__END__
foo bar baz\

" =>
    [],

  "__FILE__" => [[:str, "(string)"]],
  "__FILE__;a" =>
    [[:vcall, :a],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "__LINE__" => [[:lit, 1]],
  "__LINE__;a" =>
    [[:vcall, :a],
    {:warnings=>["(string):1: warning: unused literal ignored"]}],

  "\
\\
__END__\

" =>
    [],

  "`\#{}`" => [[:dxstr, "", [:evstr]]],
  "`a b \#{c}`" => [[:dxstr, "a b ", [:evstr, [:vcall, :c]]]],
  "`a\#{\"b\"}c\#{__FILE__}e\#{\"f\"}`" =>
    [[:xstr, "abc(string)ef"]],

  "`a`" => [[:xstr, "a"]],
  "`a`; b" => [[:block, [:xstr, "a"], [:vcall, :b]]],
  "a" => [[:vcall, :a]],
  "a !~ b" =>
    [[:not, [:call, [:vcall, :a], :=~, [:array, [:vcall, :b]]]]],

  "a & b" => [[:call, [:vcall, :a], :&, [:array, [:vcall, :b]]]],
  "a && b" => [[:and, [:vcall, :a], [:vcall, :b]]],
  "a && b and c" =>
    [[:and, [:vcall, :a], [:and, [:vcall, :b], [:vcall, :c]]]],

  "a &&b" => [[:and, [:vcall, :a], [:vcall, :b]]],
  "a &b" =>
    [[:block_pass, [:vcall, :b], [:fcall, :a]],
    {:warnings=>["(string):1: warning: `&' interpreted as argument prefix"]}],

  "a * b" => [[:call, [:vcall, :a], :*, [:array, [:vcall, :b]]]],
  "a *b" =>
    [[:fcall, :a, [:splat, [:vcall, :b]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "a + b" => [[:call, [:vcall, :a], :+, [:array, [:vcall, :b]]]],
  "a +b" =>
    [[:fcall, :a, [:array, [:call, [:vcall, :b], :+@]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "a - b" => [[:call, [:vcall, :a], :-, [:array, [:vcall, :b]]]],
  "a -b" =>
    [[:fcall, :a, [:array, [:call, [:vcall, :b], :-@]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "a << b" => [[:call, [:vcall, :a], :<<, [:array, [:vcall, :b]]]],
  "a = (p 1,g) rescue 2" =>
    [[:lasgn, :a, [:rescue, [:fcall, :p, [:array, [:lit, 1], [
    :vcall, :g]]], [:resbody, nil, [:lit, 2]]]]],

  "a = (p(1)) rescue 2" =>
    [[:lasgn, :a, [:rescue, [:fcall, :p, [:array, [:lit, 1]]], [
    :resbody, nil, [:lit, 2]]]]],

  "a = (p(1,g)) rescue 2" =>
    [[:lasgn, :a, [:rescue, [:fcall, :p, [:array, [:lit, 1], [
    :vcall, :g]]], [:resbody, nil, [:lit, 2]]]]],

  "a = (p) rescue 2" =>
    [[:lasgn, :a, [:rescue, [:vcall, :p], [:resbody, nil, [:lit,
     2]]]]],

  "a = (r e +t(j)) rescue 2" =>
    [[:lasgn, :a, [:rescue, [:fcall, :r, [:array, [:fcall, :e, [
    :array, [:call, [:fcall, :t, [:array, [:vcall, :j]]], :+@]]]
    ]], [:resbody, nil, [:lit, 2]]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces", "(string):1: warning: parenthesize argument(s) for future version"]}],

  "a = (r(e) +t(j)) rescue 2" =>
    [[:lasgn, :a, [:rescue, [:call, [:fcall, :r, [:array, [
    :vcall, :e]]], :+, [:array, [:fcall, :t, [:array, [:vcall, :j]
    ]]]], [:resbody, nil, [:lit, 2]]]]],

  "a = p 1,g rescue 2" =>
    [[:rescue, [:lasgn, :a, [:fcall, :p, [:array, [:lit, 1], [
    :vcall, :g]]]], [:resbody, nil, [:lit, 2]]]],

  "a = p rescue 2" =>
    [[:lasgn, :a, [:rescue, [:vcall, :p], [:resbody, nil, [:lit,
     2]]]]],

  "a = p rescue b" =>
    [[:lasgn, :a, [:rescue, [:vcall, :p], [:resbody, nil, [
    :vcall, :b]]]]],

  "a = p(1) rescue 2" =>
    [[:lasgn, :a, [:rescue, [:fcall, :p, [:array, [:lit, 1]]], [
    :resbody, nil, [:lit, 2]]]]],

  "a = p(1,g) rescue 2" =>
    [[:lasgn, :a, [:rescue, [:fcall, :p, [:array, [:lit, 1], [
    :vcall, :g]]], [:resbody, nil, [:lit, 2]]]]],

  "a = r e +t(j) rescue 2" =>
    [[:rescue, [:lasgn, :a, [:fcall, :r, [:array, [:fcall, :e, [
    :array, [:call, [:fcall, :t, [:array, [:vcall, :j]]], :+@]]]
    ]]], [:resbody, nil, [:lit, 2]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces", "(string):1: warning: parenthesize argument(s) for future version"]}],

  "a = r(e) +t(j) rescue 2" =>
    [[:lasgn, :a, [:rescue, [:call, [:fcall, :r, [:array, [
    :vcall, :e]]], :+, [:array, [:fcall, :t, [:array, [:vcall, :j]
    ]]]], [:resbody, nil, [:lit, 2]]]]],

  "a =p (1).m,z=c rescue b" =>
    [[:lasgn, :a, [:svalue, [:array, [:call, [:fcall, :p, [:array, [
    :lit, 1]]], :m], [:lasgn, :z, [:rescue, [:vcall, :c], [
    :resbody, nil, [:vcall, :b]]]]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "a ? b : c" => [[:if, [:vcall, :a], [:vcall, :b], [:vcall, :c]]],
  "a ? defined? b : c" =>
    [[:if, [:vcall, :a], [:defined, [:vcall, :b]], [:vcall, :c]]
    ],

  "a and (b and c)" =>
    [[:and, [:vcall, :a], [:and, [:vcall, :b], [:vcall, :c]]]],

  "a and b" => [[:and, [:vcall, :a], [:vcall, :b]]],
  "a and b && c" =>
    [[:and, [:vcall, :a], [:and, [:vcall, :b], [:vcall, :c]]]],

  "a and b and c" =>
    [[:and, [:vcall, :a], [:and, [:vcall, :b], [:vcall, :c]]]],

  "a and b or c" =>
    [[:or, [:and, [:vcall, :a], [:vcall, :b]], [:vcall, :c]]],

  "a b c" =>
    [[:fcall, :a, [:array, [:fcall, :b, [:array, [:vcall, :c]]]]
    ],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "a b do c end" =>
    [[:iter, [:fcall, :a, [:array, [:vcall, :b]]], nil, [:vcall,
     :c]]],

  "a b do end" =>
    [[:iter, [:fcall, :a, [:array, [:vcall, :b]]], nil]],

  "a b() do end" =>
    [[:iter, [:fcall, :a, [:array, [:fcall, :b]]], nil]],

  "a b,*c  do c end" =>
    [[:iter, [:fcall, :a, [:argscat, [:array, [:vcall, :b]], [
    :vcall, :c]]], nil, [:vcall, :c]]],

  "a b,*c {c}" =>
    [[:fcall, :a, [:argscat, [:array, [:vcall, :b]], [:iter, [
    :fcall, :c], nil, [:vcall, :c]]]]],

  "a b,c do c end" =>
    [[:iter, [:fcall, :a, [:array, [:vcall, :b], [:vcall, :c]]],
     nil, [:vcall, :c]]],

  "a b,c {c}" =>
    [[:fcall, :a, [:array, [:vcall, :b], [:iter, [:fcall, :c], nil, [
    :vcall, :c]]]]],

  "a b.c do end" =>
    [[:iter, [:fcall, :a, [:array, [:call, [:vcall, :b], :c]]],
     nil]],

  "a b.c() do end" =>
    [[:iter, [:fcall, :a, [:array, [:call, [:vcall, :b], :c]]],
     nil]],

  "a b=c do end" =>
    [[:iter, [:fcall, :a, [:array, [:lasgn, :b, [:vcall, :c]]]],
     nil]],

  "a b=c,d=e do end" =>
    [[:iter, [:fcall, :a, [:array, [:lasgn, :b, [:vcall, :c]], [
    :lasgn, :d, [:vcall, :e]]]], nil]],

  "a b=c,d=e do f end" =>
    [[:iter, [:fcall, :a, [:array, [:lasgn, :b, [:vcall, :c]], [
    :lasgn, :d, [:vcall, :e]]]], nil, [:vcall, :f]]],

  "a b=c,d=e,&f" =>
    [[:block_pass, [:vcall, :f], [:fcall, :a, [:array, [:lasgn,
     :b, [:vcall, :c]], [:lasgn, :d, [:vcall, :e]]]]]],

  "a b=c,d=e,*f do end" =>
    [[:iter, [:fcall, :a, [:argscat, [:array, [:lasgn, :b, [
    :vcall, :c]], [:lasgn, :d, [:vcall, :e]]], [:vcall, :f]]], nil]
    ],

  "a b=c,d=e,*f do g end" =>
    [[:iter, [:fcall, :a, [:argscat, [:array, [:lasgn, :b, [
    :vcall, :c]], [:lasgn, :d, [:vcall, :e]]], [:vcall, :f]]], nil, [
    :vcall, :g]]],

  "a b=c,d=e,*f,&g" =>
    [[:block_pass, [:vcall, :g], [:fcall, :a, [:argscat, [:array, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:vcall, :e]]], [
    :vcall, :f]]]]],

  "a b{c}" =>
    [[:fcall, :a, [:array, [:iter, [:fcall, :b], nil, [:vcall, :c]
    ]]]],

  "a do b end" => [[:iter, [:fcall, :a], nil, [:vcall, :b]]],
  "a if 1" =>
    [[:if, [:lit, 1], [:vcall, :a], nil],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "a if 1..2" =>
    [[:if, [:flip2, [:call, [:lit, 1], :==, [:array, [:gvar, :$.]
    ]], [:call, [:lit, 2], :==, [:array, [:gvar, :$.]]]], [
    :vcall, :a], nil],
    {:warnings=>["(string):1: warning: integer literal in conditional range", "(string):1: warning: integer literal in conditional range"]}],

  "a if b" => [[:if, [:vcall, :b], [:vcall, :a], nil]],
  "a or (b or c)" =>
    [[:or, [:vcall, :a], [:or, [:vcall, :b], [:vcall, :c]]]],

  "a or b" => [[:or, [:vcall, :a], [:vcall, :b]]],
  "a or b and c" =>
    [[:and, [:or, [:vcall, :a], [:vcall, :b]], [:vcall, :c]]],

  "a or b or c" =>
    [[:or, [:vcall, :a], [:or, [:vcall, :b], [:vcall, :c]]]],

  "a or b || c" =>
    [[:or, [:vcall, :a], [:or, [:vcall, :b], [:vcall, :c]]]],

  "a rescue \"\#{a=b,d rescue c}\"" =>
    [[:rescue, [:vcall, :a], [:resbody, nil, [:dstr, "", [:evstr, [
    :rescue, [:lasgn, :a, [:svalue, [:array, [:vcall, :b], [
    :vcall, :d]]]], [:resbody, nil, [:vcall, :c]]]]]]]],

  "a rescue (b;c)" =>
    [[:rescue, [:vcall, :a], [:resbody, nil, [:block, [:vcall, :b], [
    :vcall, :c]]]]],

  "a rescue * @com_disk" => SyntaxError.new("(string):1: syntax error, unexpected $end, expecting '='\na rescue * @com_disk\n                    ^"),
  "a rescue - @com_disk" =>
    [[:rescue, [:vcall, :a], [:resbody, nil, [:call, [
    :ivar, :@com_disk], :-@]]]],

  "a rescue BEGIN {a}" =>
    [[:rescue, [:vcall, :a], [:resbody, nil]]],

  "a rescue BEGIN{b}" =>
    [[:rescue, [:vcall, :a], [:resbody, nil]]],

  "a rescue a rescue a=1,2 rescue 4" =>
    [[:rescue, [:rescue, [:rescue, [:vcall, :a], [:resbody, nil,
     [:vcall, :a]]], [:resbody, nil, [:lasgn, :a, [:svalue, [
    :array, [:lit, 1], [:lit, 2]]]]]], [:resbody, nil, [:lit, 4]
    ]]],

  "a rescue a0 = b 1 do end rescue c" =>
    [[:rescue, [:rescue, [:vcall, :a], [:resbody, nil, [:lasgn,
     :a0, [:iter, [:fcall, :b, [:array, [:lit, 1]]], nil]]]], [
    :resbody, nil, [:vcall, :c]]]],

  "a rescue a=*b rescue c" =>
    [[:rescue, [:rescue, [:vcall, :a], [:resbody, nil, [:lasgn,
     :a, [:svalue, [:splat, [:vcall, :b]]]]]], [:resbody, nil, [
    :vcall, :c]]]],

  "a rescue a=*b rescue c rescue d" =>
    [[:rescue, [:rescue, [:rescue, [:vcall, :a], [:resbody, nil,
     [:lasgn, :a, [:svalue, [:splat, [:vcall, :b]]]]]], [:resbody,
     nil, [:vcall, :c]]], [:resbody, nil, [:vcall, :d]]]],

  "a rescue a=1,2 rescue 4" =>
    [[:rescue, [:rescue, [:vcall, :a], [:resbody, nil, [:lasgn,
     :a, [:svalue, [:array, [:lit, 1], [:lit, 2]]]]]], [:resbody,
     nil, [:lit, 4]]]],

  "a rescue b" =>
    [[:rescue, [:vcall, :a], [:resbody, nil, [:vcall, :b]]]],

  "a rescue b and c" =>
    [[:rescue, [:vcall, :a], [:resbody, nil, [:and, [:vcall, :b], [
    :vcall, :c]]]]],

  "a rescue b if c" =>
    [[:if, [:vcall, :c], [:rescue, [:vcall, :a], [:resbody, nil,
     [:vcall, :b]]], nil]],

  "a rescue b or c" =>
    [[:rescue, [:vcall, :a], [:resbody, nil, [:or, [:vcall, :b],
     [:vcall, :c]]]]],

  "a rescue b until 1" =>
    [[:until, [:lit, 1], [:rescue, [:vcall, :a], [:resbody, nil,
     [:vcall, :b]]], true],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "a rescue b while 1" =>
    [[:while, [:lit, 1], [:rescue, [:vcall, :a], [:resbody, nil,
     [:vcall, :b]]], true],
    {:warnings=>["(string):1: warning: literal in condition"]}],

  "a rescue b.c" =>
    [[:rescue, [:vcall, :a], [:resbody, nil, [:call, [:vcall, :b],
     :c]]]],

  "a rescue b;c" =>
    [[:block, [:rescue, [:vcall, :a], [:resbody, nil, [:vcall, :b]
    ]], [:vcall, :c]]],

  "a rescue begin; p (1..10).method(:each); rescue b; end.m" =>
    [[:rescue, [:vcall, :a], [:resbody, nil, [:call, [:rescue, [
    :fcall, :p, [:array, [:call, [:lit, 1..10], :method, [:array, [
    :lit, :each]]]]], [:resbody, [:array, [:vcall, :b]]]], :m]]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "a unless b" => [[:if, [:vcall, :b], nil, [:vcall, :a]]],
  "a until b" => [[:until, [:vcall, :b], [:vcall, :a], true]],
  "a while b" => [[:while, [:vcall, :b], [:vcall, :a], true]],
  "a || b or c" =>
    [[:or, [:vcall, :a], [:or, [:vcall, :b], [:vcall, :c]]]],

  "a!=b" =>
    [[:not, [:call, [:vcall, :a], :==, [:array, [:vcall, :b]]]]],

  "a!`b" => SyntaxError.new("(string):1: unterminated string meets end of file"),
  "a!~b" => [[:fcall, :a!, [:array, [:call, [:vcall, :b], :~]]]],
  "a%=b" =>
    [[:lasgn, :a, [:call, [:lvar, :a], :%, [:array, [:vcall, :b]
    ]]]],

  "a& b" => [[:call, [:vcall, :a], :&, [:array, [:vcall, :b]]]],
  "a&& b" => [[:and, [:vcall, :a], [:vcall, :b]]],
  "a&&(b&&c)" =>
    [[:and, [:vcall, :a], [:and, [:vcall, :b], [:vcall, :c]]]],

  "a&&=b" =>
    [[:op_asgn_and, [:lvar, :a], [:lasgn, :a, [:vcall, :b]]]],

  "a&&b" => [[:and, [:vcall, :a], [:vcall, :b]]],
  "a&&b&&c" =>
    [[:and, [:vcall, :a], [:and, [:vcall, :b], [:vcall, :c]]]],

  "a&&b||c" =>
    [[:or, [:and, [:vcall, :a], [:vcall, :b]], [:vcall, :c]]],

  "a&b" => [[:call, [:vcall, :a], :&, [:array, [:vcall, :b]]]],
  "a(&b)" => [[:block_pass, [:vcall, :b], [:fcall, :a]]],
  "a()" => [[:fcall, :a]],
  "a(){b}" => [[:iter, [:fcall, :a], nil, [:vcall, :b]]],
  "a(*b)" => [[:fcall, :a, [:splat, [:vcall, :b]]]],
  "a(b)" => [[:fcall, :a, [:array, [:vcall, :b]]]],
  "a(b).c%=d" =>
    [[:op_asgn2, [:fcall, :a, [:array, [:vcall, :b]]], :c=, :%, [
    :vcall, :d]]],

  "a(b){c}" =>
    [[:iter, [:fcall, :a, [:array, [:vcall, :b]]], nil, [:vcall,
     :c]]],

  "a(b,&c)" =>
    [[:block_pass, [:vcall, :c], [:fcall, :a, [:array, [:vcall,
     :b]]]]],

  "a(b,*c)" =>
    [[:fcall, :a, [:argscat, [:array, [:vcall, :b]], [:vcall, :c]
    ]]],

  "a(b,*c){c}" =>
    [[:iter, [:fcall, :a, [:argscat, [:array, [:vcall, :b]], [
    :vcall, :c]]], nil, [:vcall, :c]]],

  "a(b,*c,&c)" =>
    [[:block_pass, [:vcall, :c], [:fcall, :a, [:argscat, [:array, [
    :vcall, :b]], [:vcall, :c]]]]],

  "a(b,c){c}" =>
    [[:iter, [:fcall, :a, [:array, [:vcall, :b], [:vcall, :c]]],
     nil, [:vcall, :c]]],

  "a(b,c,&c)" =>
    [[:block_pass, [:vcall, :c], [:fcall, :a, [:array, [:vcall,
     :b], [:vcall, :c]]]]],

  "a* b" => [[:call, [:vcall, :a], :*, [:array, [:vcall, :b]]]],
  "a**b**c" =>
    [[:call, [:vcall, :a], :**, [:array, [:call, [:vcall, :b], :**, [
    :array, [:vcall, :c]]]]]],

  "a*b" => [[:call, [:vcall, :a], :*, [:array, [:vcall, :b]]]],
  "a+ b" => [[:call, [:vcall, :a], :+, [:array, [:vcall, :b]]]],
  "a+=b rescue c" =>
    [[:rescue, [:lasgn, :a, [:call, [:lvar, :a], :+, [:array, [
    :vcall, :b]]]], [:resbody, nil, [:vcall, :c]]]],

  "a+=b+=1" =>
    [[:lasgn, :a, [:call, [:lvar, :a], :+, [:array, [:lasgn, :b,
     [:call, [:lvar, :b], :+, [:array, [:lit, 1]]]]]]]],

  "a+=b=1" =>
    [[:lasgn, :a, [:call, [:lvar, :a], :+, [:array, [:lasgn, :b,
     [:lit, 1]]]]]],

  "a+B::c=d" =>
    [[:call, [:vcall, :a], :+, [:array, [:attrasgn, [:const, :B],
     :c=, [:array, [:vcall, :d]]]]]],

  "a+b" => [[:call, [:vcall, :a], :+, [:array, [:vcall, :b]]]],
  "a+b.c=d" =>
    [[:call, [:vcall, :a], :+, [:array, [:attrasgn, [:vcall, :b],
     :c=, [:array, [:vcall, :d]]]]]],

  "a+b=c" =>
    [[:call, [:vcall, :a], :+, [:array, [:lasgn, :b, [:vcall, :c]
    ]]]],

  "a+b=c rescue d" =>
    [[:call, [:vcall, :a], :+, [:array, [:lasgn, :b, [:rescue, [
    :vcall, :c], [:resbody, nil, [:vcall, :d]]]]]]],

  "a+b[c]=d" =>
    [[:call, [:vcall, :a], :+, [:array, [:attrasgn, [:vcall, :b],
     :[]=, [:array, [:vcall, :c], [:vcall, :d]]]]]],

  "\
a,(%
__END__
[a]).w,c=d,e,f" =>
    [[:masgn, [:array, [:lasgn, :a], [:attrasgn, [:call, [:str,
     "__END__"], :[], [:array, [:lvar, :a]]], :w=], [:lasgn, :c]
    ], nil, [:array, [:vcall, :d], [:vcall, :e], [:vcall, :f]]]],

  "a,((BEGIN {})).w,c=d,e,f" =>
    [[:masgn, [:array, [:lasgn, :a], [:attrasgn, [:nil], :w=], [
    :lasgn, :c]], nil, [:array, [:vcall, :d], [:vcall, :e], [
    :vcall, :f]]]],

  "a,(* =f,g rescue b and c).w,c=d,e,f" =>
    [[:masgn, [:array, [:lasgn, :a], [:attrasgn, [:rescue, [
    :masgn, nil, [:splat], [:array, [:vcall, :f], [:vcall, :g]]
    ], [:resbody, nil, [:and, [:vcall, :b], [:vcall, :c]]]], :w=], [
    :lasgn, :c]], nil, [:array, [:vcall, :d], [:vcall, :e], [
    :vcall, :f]]]],

  "a,(a0 rescue b0=b,d rescue c).w=d" =>
    [[:masgn, [:array, [:lasgn, :a], [:attrasgn, [:rescue, [
    :rescue, [:vcall, :a0], [:resbody, nil, [:lasgn, :b0, [
    :svalue, [:array, [:vcall, :b], [:vcall, :d]]]]]], [:resbody,
     nil, [:vcall, :c]]], :w=]], nil, [:to_ary, [:vcall, :d]]]],

  "a,* =1" =>
    [[:masgn, [:array, [:lasgn, :a]], [:splat], [:to_ary, [:lit,
     1]]]],

  "a,* =b rescue c" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :a]], [:splat], [
    :to_ary, [:vcall, :b]]], [:resbody, nil, [:vcall, :c]]]],

  "a,*b=c,d=e,*f" =>
    [[:masgn, [:array, [:lasgn, :a]], [:lasgn, :b], [:argscat, [
    :array, [:vcall, :c], [:lasgn, :d, [:vcall, :e]]], [:vcall,
     :f]]]],

  "a,*d=b rescue c" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :a]], [:lasgn, :d], [
    :to_ary, [:vcall, :b]]], [:resbody, nil, [:vcall, :c]]]],

  "a,*k=p,z=c rescue b" =>
    [[:masgn, [:array, [:lasgn, :a]], [:lasgn, :k], [:array, [
    :vcall, :p], [:lasgn, :z, [:rescue, [:vcall, :c], [:resbody,
     nil, [:vcall, :b]]]]]]],

  "a,=1" =>
    [[:masgn, [:array, [:lasgn, :a]], nil, [:to_ary, [:lit, 1]]]
    ],

  "a,=b rescue c" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :a]], nil, [:to_ary, [
    :vcall, :b]]], [:resbody, nil, [:vcall, :c]]]],

  "a,b,* =1" =>
    [[:masgn, [:array, [:lasgn, :a], [:lasgn, :b]], [:splat], [
    :to_ary, [:lit, 1]]]],

  "a,b,=1" =>
    [[:masgn, [:array, [:lasgn, :a], [:lasgn, :b]], nil, [:to_ary, [
    :lit, 1]]]],

  "a,b=(*c=b,a)" =>
    [[:masgn, [:array, [:lasgn, :a], [:lasgn, :b]], nil, [:to_ary, [
    :masgn, nil, [:lasgn, :c], [:array, [:lvar, :b], [:lvar, :a]
    ]]]]],

  "a,b=c,d" =>
    [[:masgn, [:array, [:lasgn, :a], [:lasgn, :b]], nil, [:array, [
    :vcall, :c], [:vcall, :d]]]],

  "a,b=c,d=e,f" =>
    [[:masgn, [:array, [:lasgn, :a], [:lasgn, :b]], nil, [:array, [
    :vcall, :c], [:lasgn, :d, [:vcall, :e]], [:vcall, :f]]]],

  "a,d=b rescue c" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :a], [:lasgn, :d]], nil, [
    :to_ary, [:vcall, :b]]], [:resbody, nil, [:vcall, :c]]]],

  "a,g = p rescue b" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :a], [:lasgn, :g]], nil, [
    :to_ary, [:vcall, :p]]], [:resbody, nil, [:vcall, :b]]]],

  "a,k=p,z=c rescue b" =>
    [[:masgn, [:array, [:lasgn, :a], [:lasgn, :k]], nil, [:array, [
    :vcall, :p], [:lasgn, :z, [:rescue, [:vcall, :c], [:resbody,
     nil, [:vcall, :b]]]]]]],

  "a- b" => [[:call, [:vcall, :a], :-, [:array, [:vcall, :b]]]],
  "a-b" => [[:call, [:vcall, :a], :-, [:array, [:vcall, :b]]]],
  "a...b" => [[:dot3, [:vcall, :a], [:vcall, :b]]],
  "a..b" => [[:dot2, [:vcall, :a], [:vcall, :b]]],
  "a..b and c" =>
    [[:and, [:dot2, [:vcall, :a], [:vcall, :b]], [:vcall, :c]]],

  "a..b or c" =>
    [[:or, [:dot2, [:vcall, :a], [:vcall, :b]], [:vcall, :c]]],

  "a.BEGIN {}" => [[:iter, [:call, [:vcall, :a], :BEGIN], nil]],
  "a.END {}" => [[:iter, [:call, [:vcall, :a], :END], nil]],
  "a.b" => [[:call, [:vcall, :a], :b]],
  "a.b c" => [[:call, [:vcall, :a], :b, [:array, [:vcall, :c]]]],
  "a.b do end" => [[:iter, [:call, [:vcall, :a], :b], nil]],
  "a.b%=c" => [[:op_asgn2, [:vcall, :a], :b=, :%, [:vcall, :c]]],
  "a.b%c" =>
    [[:call, [:call, [:vcall, :a], :b], :%, [:array, [:vcall, :c]
    ]]],

  "a.b,c.d=1,2" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :a], :b=], [:attrasgn, [
    :vcall, :c], :d=]], nil, [:array, [:lit, 1], [:lit, 2]]]],

  "a.b.c%=d" =>
    [[:op_asgn2, [:call, [:vcall, :a], :b], :c=, :%, [:vcall, :d]
    ]],

  "a.b=~c" =>
    [[:call, [:call, [:vcall, :a], :b], :=~, [:array, [:vcall, :c]
    ]]],

  "a.b[3]" =>
    [[:call, [:call, [:vcall, :a], :b], :[], [:array, [:lit, 3]]
    ]],

  "a.b[]" => [[:call, [:call, [:vcall, :a], :b], :[]]],
  "a.b[]=a=b,c=d" =>
    [[:attrasgn, [:call, [:vcall, :a], :b], :[]=, [:array, [
    :svalue, [:array, [:lasgn, :a, [:vcall, :b]], [:lasgn, :c, [
    :vcall, :d]]]]]]],

  "a.b[]=d" =>
    [[:attrasgn, [:call, [:vcall, :a], :b], :[]=, [:array, [
    :vcall, :d]]]],

  "a.b[c,c2]%=d" =>
    [[:op_asgn1, [:call, [:vcall, :a], :b], [:array, [:vcall, :c], [
    :vcall, :c2]], :%, [:vcall, :d]]],

  "a.b[c,c2]=d" =>
    [[:attrasgn, [:call, [:vcall, :a], :b], :[]=, [:array, [
    :vcall, :c], [:vcall, :c2], [:vcall, :d]]]],

  "a.b[c]%=d" =>
    [[:op_asgn1, [:call, [:vcall, :a], :b], [:array, [:vcall, :c]
    ], :%, [:vcall, :d]]],

  "a.b[c]=d" =>
    [[:attrasgn, [:call, [:vcall, :a], :b], :[]=, [:array, [
    :vcall, :c], [:vcall, :d]]]],

  "a.break" => [[:call, [:vcall, :a], :break]],
  "a.break % 1 " =>
    [[:call, [:call, [:vcall, :a], :break], :%, [:array, [:lit,
     1]]]],

  "a.break / 1 /" => SyntaxError.new("(string):1: syntax error, unexpected $end\na.break / 1 /\n             ^"),
  "a.break {b}" =>
    [[:iter, [:call, [:vcall, :a], :break], nil, [:vcall, :b]]],

  "a.break {}" => [[:iter, [:call, [:vcall, :a], :break], nil]],
  "a.break(&a)" =>
    [[:block_pass, [:vcall, :a], [:call, [:vcall, :a], :break]]],

  "a.break()" => [[:call, [:vcall, :a], :break]],
  "a.break(*a)" =>
    [[:call, [:vcall, :a], :break, [:splat, [:vcall, :a]]]],

  "a.break(*a,&b)" =>
    [[:block_pass, [:vcall, :b], [:call, [:vcall, :a], :break, [
    :splat, [:vcall, :a]]]]],

  "a.break(1)" =>
    [[:call, [:vcall, :a], :break, [:array, [:lit, 1]]]],

  "a.break(1,&b)" =>
    [[:block_pass, [:vcall, :b], [:call, [:vcall, :a], :break, [
    :array, [:lit, 1]]]]],

  "a.break(1,*b)" =>
    [[:call, [:vcall, :a], :break, [:argscat, [:array, [:lit, 1]
    ], [:vcall, :b]]]],

  "a.break(1,*b,&c)" =>
    [[:block_pass, [:vcall, :c], [:call, [:vcall, :a], :break, [
    :argscat, [:array, [:lit, 1]], [:vcall, :b]]]]],

  "a.break(1,2)" =>
    [[:call, [:vcall, :a], :break, [:array, [:lit, 1], [:lit, 2]
    ]]],

  "a.break(1,2,&c)" =>
    [[:block_pass, [:vcall, :c], [:call, [:vcall, :a], :break, [
    :array, [:lit, 1], [:lit, 2]]]]],

  "a.break(1,2,*c)" =>
    [[:call, [:vcall, :a], :break, [:argscat, [:array, [:lit, 1], [
    :lit, 2]], [:vcall, :c]]]],

  "a.break(1,2,*c,&d)" =>
    [[:block_pass, [:vcall, :d], [:call, [:vcall, :a], :break, [
    :argscat, [:array, [:lit, 1], [:lit, 2]], [:vcall, :c]]]]],

  "a.break(1,2,3)" =>
    [[:call, [:vcall, :a], :break, [:array, [:lit, 1], [:lit, 2], [
    :lit, 3]]]],

  "a.break[1]" =>
    [[:call, [:call, [:vcall, :a], :break], :[], [:array, [:lit,
     1]]]],

  "a.b||=c" =>
    [[:op_asgn2, [:vcall, :a], :b=, :"||", [:vcall, :c]]],

  "a.continue[1]" =>
    [[:call, [:call, [:vcall, :a], :continue], :[], [:array, [
    :lit, 1]]]],

  "a.d=b rescue c" =>
    [[:attrasgn, [:vcall, :a], :d=, [:array, [:rescue, [:vcall,
     :b], [:resbody, nil, [:vcall, :c]]]]]],

  "a.next" => [[:call, [:vcall, :a], :next]],
  "a.next(&a)" =>
    [[:block_pass, [:vcall, :a], [:call, [:vcall, :a], :next]]],

  "a.next()" => [[:call, [:vcall, :a], :next]],
  "a.next(*a)" =>
    [[:call, [:vcall, :a], :next, [:splat, [:vcall, :a]]]],

  "a.next(*a,&b)" =>
    [[:block_pass, [:vcall, :b], [:call, [:vcall, :a], :next, [
    :splat, [:vcall, :a]]]]],

  "a.next(1)" =>
    [[:call, [:vcall, :a], :next, [:array, [:lit, 1]]]],

  "a.next(1,&b)" =>
    [[:block_pass, [:vcall, :b], [:call, [:vcall, :a], :next, [
    :array, [:lit, 1]]]]],

  "a.next(1,*b)" =>
    [[:call, [:vcall, :a], :next, [:argscat, [:array, [:lit, 1]
    ], [:vcall, :b]]]],

  "a.next(1,*b,&c)" =>
    [[:block_pass, [:vcall, :c], [:call, [:vcall, :a], :next, [
    :argscat, [:array, [:lit, 1]], [:vcall, :b]]]]],

  "a.next(1,2)" =>
    [[:call, [:vcall, :a], :next, [:array, [:lit, 1], [:lit, 2]]
    ]],

  "a.next(1,2,&c)" =>
    [[:block_pass, [:vcall, :c], [:call, [:vcall, :a], :next, [
    :array, [:lit, 1], [:lit, 2]]]]],

  "a.next(1,2,*c)" =>
    [[:call, [:vcall, :a], :next, [:argscat, [:array, [:lit, 1],
     [:lit, 2]], [:vcall, :c]]]],

  "a.next(1,2,*c,&d)" =>
    [[:block_pass, [:vcall, :d], [:call, [:vcall, :a], :next, [
    :argscat, [:array, [:lit, 1], [:lit, 2]], [:vcall, :c]]]]],

  "a.next(1,2,3)" =>
    [[:call, [:vcall, :a], :next, [:array, [:lit, 1], [:lit, 2],
     [:lit, 3]]]],

  "a.next[1]" =>
    [[:call, [:call, [:vcall, :a], :next], :[], [:array, [:lit,
     1]]]],

  "a.p ().method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p], :method, [:array, [:lit,
     :each]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "a.p (1..10).method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p, [:array, [:lit, 1..10]]],
     :method, [:array, [:lit, :each]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "a.p (1..10,1).method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p, [:array, [:lit, 1..10], [
    :lit, 1]]], :method, [:array, [:lit, :each]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "a.p p (1).m" =>
    [[:call, [:vcall, :a], :p, [:array, [:call, [:fcall, :p, [
    :array, [:lit, 1]]], :m]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "a.p().method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p], :method, [:array, [:lit,
     :each]]]],

  "a.p(1..10).method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p, [:array, [:lit, 1..10]]],
     :method, [:array, [:lit, :each]]]],

  "a.p(1..10,1).method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p, [:array, [:lit, 1..10], [
    :lit, 1]]], :method, [:array, [:lit, :each]]]],

  "a.return" => [[:call, [:vcall, :a], :return]],
  "a.return % 1 " =>
    [[:call, [:call, [:vcall, :a], :return], :%, [:array, [:lit,
     1]]]],

  "a.return - @com_disk" =>
    [[:call, [:call, [:vcall, :a], :return], :-, [:array, [:ivar,
     :@com_disk]]]],

  "a.return / 1 /" => SyntaxError.new("(string):1: syntax error, unexpected $end\na.return / 1 /\n              ^"),
  "a.return {b}" =>
    [[:iter, [:call, [:vcall, :a], :return], nil, [:vcall, :b]]],

  "a.return {}" => [[:iter, [:call, [:vcall, :a], :return], nil]],
  "a.return(&a)" =>
    [[:block_pass, [:vcall, :a], [:call, [:vcall, :a], :return]]
    ],

  "a.return()" => [[:call, [:vcall, :a], :return]],
  "a.return(*a)" =>
    [[:call, [:vcall, :a], :return, [:splat, [:vcall, :a]]]],

  "a.return(*a,&b)" =>
    [[:block_pass, [:vcall, :b], [:call, [:vcall, :a], :return, [
    :splat, [:vcall, :a]]]]],

  "a.return(1)" =>
    [[:call, [:vcall, :a], :return, [:array, [:lit, 1]]]],

  "a.return(1,&b)" =>
    [[:block_pass, [:vcall, :b], [:call, [:vcall, :a], :return, [
    :array, [:lit, 1]]]]],

  "a.return(1,*b)" =>
    [[:call, [:vcall, :a], :return, [:argscat, [:array, [:lit, 1]
    ], [:vcall, :b]]]],

  "a.return(1,*b,&c)" =>
    [[:block_pass, [:vcall, :c], [:call, [:vcall, :a], :return, [
    :argscat, [:array, [:lit, 1]], [:vcall, :b]]]]],

  "a.return(1,2)" =>
    [[:call, [:vcall, :a], :return, [:array, [:lit, 1], [:lit, 2]
    ]]],

  "a.return(1,2,&c)" =>
    [[:block_pass, [:vcall, :c], [:call, [:vcall, :a], :return, [
    :array, [:lit, 1], [:lit, 2]]]]],

  "a.return(1,2,*c)" =>
    [[:call, [:vcall, :a], :return, [:argscat, [:array, [:lit, 1], [
    :lit, 2]], [:vcall, :c]]]],

  "a.return(1,2,*c,&d)" =>
    [[:block_pass, [:vcall, :d], [:call, [:vcall, :a], :return, [
    :argscat, [:array, [:lit, 1], [:lit, 2]], [:vcall, :c]]]]],

  "a.return(1,2,3)" =>
    [[:call, [:vcall, :a], :return, [:array, [:lit, 1], [:lit, 2], [
    :lit, 3]]]],

  "a.return[1]" =>
    [[:call, [:call, [:vcall, :a], :return], :[], [:array, [:lit,
     1]]]],

  "a.super" => [[:call, [:vcall, :a], :super]],
  "a.super(&a)" =>
    [[:block_pass, [:vcall, :a], [:call, [:vcall, :a], :super]]],

  "a.super()" => [[:call, [:vcall, :a], :super]],
  "a.super(*a)" =>
    [[:call, [:vcall, :a], :super, [:splat, [:vcall, :a]]]],

  "a.super(*a,&b)" =>
    [[:block_pass, [:vcall, :b], [:call, [:vcall, :a], :super, [
    :splat, [:vcall, :a]]]]],

  "a.super(1)" =>
    [[:call, [:vcall, :a], :super, [:array, [:lit, 1]]]],

  "a.super(1,&b)" =>
    [[:block_pass, [:vcall, :b], [:call, [:vcall, :a], :super, [
    :array, [:lit, 1]]]]],

  "a.super(1,*b)" =>
    [[:call, [:vcall, :a], :super, [:argscat, [:array, [:lit, 1]
    ], [:vcall, :b]]]],

  "a.super(1,*b,&c)" =>
    [[:block_pass, [:vcall, :c], [:call, [:vcall, :a], :super, [
    :argscat, [:array, [:lit, 1]], [:vcall, :b]]]]],

  "a.super(1,2)" =>
    [[:call, [:vcall, :a], :super, [:array, [:lit, 1], [:lit, 2]
    ]]],

  "a.super(1,2,&c)" =>
    [[:block_pass, [:vcall, :c], [:call, [:vcall, :a], :super, [
    :array, [:lit, 1], [:lit, 2]]]]],

  "a.super(1,2,*c)" =>
    [[:call, [:vcall, :a], :super, [:argscat, [:array, [:lit, 1], [
    :lit, 2]], [:vcall, :c]]]],

  "a.super(1,2,*c,&d)" =>
    [[:block_pass, [:vcall, :d], [:call, [:vcall, :a], :super, [
    :argscat, [:array, [:lit, 1], [:lit, 2]], [:vcall, :c]]]]],

  "a.super(1,2,3)" =>
    [[:call, [:vcall, :a], :super, [:array, [:lit, 1], [:lit, 2], [
    :lit, 3]]]],

  "a.yield" => [[:call, [:vcall, :a], :yield]],
  "a.yield(&a)" =>
    [[:block_pass, [:vcall, :a], [:call, [:vcall, :a], :yield]]],

  "a.yield()" => [[:call, [:vcall, :a], :yield]],
  "a.yield(*a)" =>
    [[:call, [:vcall, :a], :yield, [:splat, [:vcall, :a]]]],

  "a.yield(*a,&b)" =>
    [[:block_pass, [:vcall, :b], [:call, [:vcall, :a], :yield, [
    :splat, [:vcall, :a]]]]],

  "a.yield(1)" =>
    [[:call, [:vcall, :a], :yield, [:array, [:lit, 1]]]],

  "a.yield(1,&b)" =>
    [[:block_pass, [:vcall, :b], [:call, [:vcall, :a], :yield, [
    :array, [:lit, 1]]]]],

  "a.yield(1,*b)" =>
    [[:call, [:vcall, :a], :yield, [:argscat, [:array, [:lit, 1]
    ], [:vcall, :b]]]],

  "a.yield(1,*b,&c)" =>
    [[:block_pass, [:vcall, :c], [:call, [:vcall, :a], :yield, [
    :argscat, [:array, [:lit, 1]], [:vcall, :b]]]]],

  "a.yield(1,2)" =>
    [[:call, [:vcall, :a], :yield, [:array, [:lit, 1], [:lit, 2]
    ]]],

  "a.yield(1,2,&c)" =>
    [[:block_pass, [:vcall, :c], [:call, [:vcall, :a], :yield, [
    :array, [:lit, 1], [:lit, 2]]]]],

  "a.yield(1,2,*c)" =>
    [[:call, [:vcall, :a], :yield, [:argscat, [:array, [:lit, 1], [
    :lit, 2]], [:vcall, :c]]]],

  "a.yield(1,2,*c,&d)" =>
    [[:block_pass, [:vcall, :d], [:call, [:vcall, :a], :yield, [
    :argscat, [:array, [:lit, 1], [:lit, 2]], [:vcall, :c]]]]],

  "a.yield(1,2,3)" =>
    [[:call, [:vcall, :a], :yield, [:array, [:lit, 1], [:lit, 2], [
    :lit, 3]]]],

  "a.~" => [[:call, [:vcall, :a], :~]],
  "a.~@" => [[:call, [:vcall, :a], :~]],
  "a0 += *a+=c" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR\na0 += *a+=c\n       ^"),
  "a0 += *a=c" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR\na0 += *a=c\n       ^"),
  "a0 += p (1).d" =>
    [[:lasgn, :a0, [:call, [:lvar, :a0], :+, [:array, [:call, [
    :fcall, :p, [:array, [:lit, 1]]], :d]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "a0 = \"\#{begin; a; rescue b; p (1..10).method(:each)}\"\
 rescue b0" =>
    SyntaxError.new("(string):1: syntax error, unexpected '}', expecting kEND\na0 = \"\#{begin; a; rescue b; p (1..10).method(:each)}\" rescue b0\n                                                    ^\n(string):1: unterminated string meets end of file"),

  "a0 = (\"\#{a=b,d rescue c}\") rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:dstr, "", [:evstr, [:rescue, [
    :lasgn, :a, [:svalue, [:array, [:vcall, :b], [:vcall, :d]]]
    ], [:resbody, nil, [:vcall, :c]]]]], [:resbody, nil, [:vcall,
     :b0]]]]],

  "a0 = (*a=c)" =>
    [[:lasgn, :a0, [:masgn, nil, [:lasgn, :a], [:array, [:vcall,
     :c]]]]],

  "a0 = (a;b;) rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:block, [:vcall, :a], [:vcall, :b]
    ], [:resbody, nil, [:vcall, :b0]]]]],

  "a0 = *(a=c)" =>
    [[:lasgn, :a0, [:svalue, [:splat, [:lasgn, :a, [:vcall, :c]]
    ]]]],

  "a0 = *a+=c" =>
    [[:lasgn, :a0, [:svalue, [:splat, [:lasgn, :a, [:call, [
    :lvar, :a], :+, [:array, [:vcall, :c]]]]]]]],

  "a0 = *a=b" =>
    [[:lasgn, :a0, [:svalue, [:splat, [:lasgn, :a, [:vcall, :b]]
    ]]]],

  "a0 = *a=c" =>
    [[:lasgn, :a0, [:svalue, [:splat, [:lasgn, :a, [:vcall, :c]]
    ]]]],

  "a0 = *t rescue b0" =>
    [[:rescue, [:lasgn, :a0, [:svalue, [:splat, [:vcall, :t]]]],
     [:resbody, nil, [:vcall, :b0]]]],

  "a0 = @data.[]=(*args) rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:call, [:ivar, :@data], :[]=, [
    :splat, [:vcall, :args]]], [:resbody, nil, [:vcall, :b0]]]]],

  "a0 = a rescue b && c" =>
    [[:lasgn, :a0, [:rescue, [:vcall, :a], [:resbody, nil, [:and, [
    :vcall, :b], [:vcall, :c]]]]]],

  "a0 = a rescue b and c" =>
    [[:and, [:lasgn, :a0, [:rescue, [:vcall, :a], [:resbody, nil, [
    :vcall, :b]]]], [:vcall, :c]]],

  "a0 = a rescue b if c" =>
    [[:if, [:vcall, :c], [:lasgn, :a0, [:rescue, [:vcall, :a], [
    :resbody, nil, [:vcall, :b]]]], nil]],

  "a0 = a rescue b or c" =>
    [[:or, [:lasgn, :a0, [:rescue, [:vcall, :a], [:resbody, nil,
     [:vcall, :b]]]], [:vcall, :c]]],

  "a0 = a rescue k ? b : d" =>
    [[:lasgn, :a0, [:rescue, [:vcall, :a], [:resbody, nil, [:if,
     [:vcall, :k], [:vcall, :b], [:vcall, :d]]]]]],

  "a0 = az=d,y" =>
    [[:lasgn, :a0, [:svalue, [:array, [:lasgn, :az, [:vcall, :d]
    ], [:vcall, :y]]]]],

  "a0 = b 1 do end rescue c" =>
    [[:rescue, [:lasgn, :a0, [:iter, [:fcall, :b, [:array, [:lit,
     1]]], nil]], [:resbody, nil, [:vcall, :c]]]],

  "a0 = b do end rescue c" =>
    [[:lasgn, :a0, [:rescue, [:iter, [:fcall, :b], nil], [
    :resbody, nil, [:vcall, :c]]]]],

  "a0 = b() do end rescue c" =>
    [[:lasgn, :a0, [:rescue, [:iter, [:fcall, :b], nil], [
    :resbody, nil, [:vcall, :c]]]]],

  "a0 = begin raise \"foo\" end rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:begin, [:fcall, :raise, [:array, [
    :str, "foo"]]]], [:resbody, nil, [:vcall, :b0]]]]],

  "a0 = begin; a; end rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:begin, [:vcall, :a]], [:resbody,
     nil, [:vcall, :b0]]]]],

  "a0 = begin; end rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:nil], [:resbody, nil, [:vcall, :b0]
    ]]]],

  "a0 = begin;rescue;end rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:begin, [:rescue, [:resbody, nil]]
    ], [:resbody, nil, [:vcall, :b0]]]]],

  "a0 = def a b,d=e; end rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:defn, :a, [:scope, [:block, [
    :args, :b, :d, [:block, [:lasgn, :d, [:vcall, :e]]]], [:nil]
    ]]], [:resbody, nil, [:vcall, :b0]]]]],

  "a0 = def a b=c,d=e; end rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:defn, :a, [:scope, [:block, [
    :args, :b, :d, [:block, [:lasgn, :b, [:vcall, :c]], [:lasgn,
     :d, [:vcall, :e]]]], [:nil]]]], [:resbody, nil, [:vcall, :b0]
    ]]]],

  "\
a0 = def a.
__END__
; end rescue b0" =>
    SyntaxError.new("(string):2: syntax error, unexpected $end\n__END__\n ^"),

  "a0 = def a.b; end rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:defs, [:vcall, :a], :b, [:scope, [
    :args]]], [:resbody, nil, [:vcall, :b0]]]]],

  "a0 = k ? a rescue b : d" => SyntaxError.new("(string):1: syntax error, unexpected kRESCUE_MOD\na0 = k ? a rescue b : d\n                 ^"),
  "a0 = m,*a=c" =>
    [[:lasgn, :a0, [:svalue, [:argscat, [:array, [:vcall, :m]], [
    :lasgn, :a, [:vcall, :c]]]]]],

  "a0 = m,*a=c rescue b0" =>
    [[:lasgn, :a0, [:svalue, [:argscat, [:array, [:vcall, :m]], [
    :lasgn, :a, [:rescue, [:vcall, :c], [:resbody, nil, [:vcall,
     :b0]]]]]]]],

  "a0 = p (1..10).method(:each)[*b]=c rescue b0" =>
    [[:lasgn, :a0, [:attrasgn, [:call, [:fcall, :p, [:array, [
    :lit, 1..10]]], :method, [:array, [:lit, :each]]], :[]=, [
    :argspush, [:splat, [:vcall, :b]], [:rescue, [:vcall, :c], [
    :resbody, nil, [:vcall, :b0]]]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "a0 = p 1.q rescue b0" =>
    [[:rescue, [:lasgn, :a0, [:fcall, :p, [:array, [:call, [:lit,
     1], :q]]]], [:resbody, nil, [:vcall, :b0]]]],

  "a0 = p 1.q,2 rescue b0" =>
    [[:rescue, [:lasgn, :a0, [:fcall, :p, [:array, [:call, [:lit,
     1], :q], [:lit, 2]]]], [:resbody, nil, [:vcall, :b0]]]],

  "a0 = ~begin; a; rescue b; end rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:call, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]], :~], [:resbody, nil, [
    :vcall, :b0]]]]],

  "a0 = ~bend" => [[:lasgn, :a0, [:call, [:vcall, :bend], :~]]],
  "a0 rescue b0=b,d rescue c" =>
    [[:rescue, [:rescue, [:vcall, :a0], [:resbody, nil, [:lasgn,
     :b0, [:svalue, [:array, [:vcall, :b], [:vcall, :d]]]]]], [
    :resbody, nil, [:vcall, :c]]]],

  "a0= *begin a end rescue b0" =>
    [[:rescue, [:lasgn, :a0, [:svalue, [:splat, [:vcall, :a]]]],
     [:resbody, nil, [:vcall, :b0]]]],

  "a0= a0 = su rescue b0 rescue b0" =>
    [[:lasgn, :a0, [:rescue, [:lasgn, :a0, [:rescue, [:vcall, :su], [
    :resbody, nil, [:vcall, :b0]]]], [:resbody, nil, [:vcall, :b0]
    ]]]],

  "a0= begin a end rescue b0,*z" => SyntaxError.new("(string):1: syntax error, unexpected ',', expecting $end\na0= begin a end rescue b0,*z\n                          ^"),
  "a0= begin a end rescue b0,z" => SyntaxError.new("(string):1: syntax error, unexpected ',', expecting $end\na0= begin a end rescue b0,z\n                          ^"),
  "a0= y,*begin a end rescue b0" =>
    [[:rescue, [:lasgn, :a0, [:svalue, [:argscat, [:array, [
    :vcall, :y]], [:vcall, :a]]]], [:resbody, nil, [:vcall, :b0]
    ]]],

  "a0= y,begin a end rescue b0" =>
    [[:rescue, [:lasgn, :a0, [:svalue, [:array, [:vcall, :y], [
    :vcall, :a]]]], [:resbody, nil, [:vcall, :b0]]]],

  "a0= y,begin a end rescue b0,z" => SyntaxError.new("(string):1: syntax error, unexpected $end, expecting '='\na0= y,begin a end rescue b0,z\n                             ^"),
  "\
a::
B\

" =>
    [[:colon2, [:vcall, :a], :B]],

  "\
a::
b\

" =>
    [[:call, [:vcall, :a], :b]],

  "a::b%=c" => [[:op_asgn2, [:vcall, :a], :b=, :%, [:vcall, :c]]],
  "a::b[3]" =>
    [[:call, [:call, [:vcall, :a], :b], :[], [:array, [:lit, 3]]
    ]],

  "a::p ().method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p], :method, [:array, [:lit,
     :each]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "a::p (1..10).method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p, [:array, [:lit, 1..10]]],
     :method, [:array, [:lit, :each]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "a::p (1..10,1).method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p, [:array, [:lit, 1..10], [
    :lit, 1]]], :method, [:array, [:lit, :each]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "a::p().method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p], :method, [:array, [:lit,
     :each]]]],

  "a::p(1..10).method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p, [:array, [:lit, 1..10]]],
     :method, [:array, [:lit, :each]]]],

  "a::p(1..10,1).method(:each)" =>
    [[:call, [:call, [:vcall, :a], :p, [:array, [:lit, 1..10], [
    :lit, 1]]], :method, [:array, [:lit, :each]]]],

  "a::~" => [[:call, [:vcall, :a], :~]],
  "a::~@" => [[:call, [:vcall, :a], :~]],
  "a<< b" => [[:call, [:vcall, :a], :<<, [:array, [:vcall, :b]]]],
  "a<<b" => [[:call, [:vcall, :a], :<<, [:array, [:vcall, :b]]]],
  "a<=>b" => [[:call, [:vcall, :a], :<=>, [:array, [:vcall, :b]]]],
  "a<=b" => [[:call, [:vcall, :a], :<=, [:array, [:vcall, :b]]]],
  "a<b" => [[:call, [:vcall, :a], :<, [:array, [:vcall, :b]]]],
  "a= y 1,2 rescue c" =>
    [[:rescue, [:lasgn, :a, [:fcall, :y, [:array, [:lit, 1], [
    :lit, 2]]]], [:resbody, nil, [:vcall, :c]]]],

  "a=\"\"; (\"a\"+\"\").b do end" =>
    [[:block, [:lasgn, :a, [:str, ""]], [:iter, [:call, [:call, [
    :str, "a"], :+, [:array, [:str, ""]]], :b], nil]]],

  "a=\"\"; (a()).b do end" =>
    [[:block, [:lasgn, :a, [:str, ""]], [:iter, [:call, [:fcall,
     :a], :b], nil]]],

  "a=\"\"; (a).b" =>
    [[:block, [:lasgn, :a, [:str, ""]], [:call, [:lvar, :a], :b]
    ]],

  "a=\"\"; (a).b do end" =>
    [[:block, [:lasgn, :a, [:str, ""]], [:iter, [:call, [:lvar,
     :a], :b], nil]]],

  "a=\"\"; a.b do end" =>
    [[:block, [:lasgn, :a, [:str, ""]], [:iter, [:call, [:lvar,
     :a], :b], nil]]],

  "a=\"\"; def (\"a\"+\"\").b; end" =>
    [[:block, [:lasgn, :a, [:str, ""]], [:defs, [:call, [:str,
     "a"], :+, [:array, [:str, ""]]], :b, [:scope, [:args]]]]],

  "a=\"\"; def (a()).b; end" =>
    [[:block, [:lasgn, :a, [:str, ""]], [:defs, [:fcall, :a], :b, [
    :scope, [:args]]]]],

  "a=\"\"; def (a).b; end" =>
    [[:block, [:lasgn, :a, [:str, ""]], [:defs, [:lvar, :a], :b,
     [:scope, [:args]]]]],

  "a=\"\"; def a.b; end" =>
    [[:block, [:lasgn, :a, [:str, ""]], [:defs, [:lvar, :a], :b,
     [:scope, [:args]]]]],

  "a=\"0\";class<<a; b end" =>
    [[:block, [:lasgn, :a, [:str, "0"]], [:sclass, [:lvar, :a], [
    :scope, [:vcall, :b]]]]],

  "a=() rescue b" =>
    [[:lasgn, :a, [:rescue, [:nil], [:resbody, nil, [:vcall, :b]
    ]]]],

  "a=(b c) rescue d" =>
    [[:lasgn, :a, [:rescue, [:fcall, :b, [:array, [:vcall, :c]]
    ], [:resbody, nil, [:vcall, :d]]]]],

  "a=(b(c)) rescue d" =>
    [[:lasgn, :a, [:rescue, [:fcall, :b, [:array, [:vcall, :c]]
    ], [:resbody, nil, [:vcall, :d]]]]],

  "a=(b) rescue c" =>
    [[:lasgn, :a, [:rescue, [:vcall, :b], [:resbody, nil, [
    :vcall, :c]]]]],

  "a=*b" => [[:lasgn, :a, [:svalue, [:splat, [:vcall, :b]]]]],
  "a=*b rescue c" =>
    [[:rescue, [:lasgn, :a, [:svalue, [:splat, [:vcall, :b]]]], [
    :resbody, nil, [:vcall, :c]]]],

  "a=*b rescue c rescue d" =>
    [[:rescue, [:rescue, [:lasgn, :a, [:svalue, [:splat, [:vcall,
     :b]]]], [:resbody, nil, [:vcall, :c]]], [:resbody, nil, [
    :vcall, :d]]]],

  "a=0;z{|a,a[b]| a.c;d}" =>
    [[:block, [:lasgn, :a, [:lit, 0]], [:iter, [:fcall, :z], [
    :masgn, [:array, [:lasgn, :a], [:attrasgn, [:lvar, :a], :[]=, [
    :array, [:vcall, :b]]]], nil, nil], [:block, [:call, [:lvar,
     :a], :c], [:vcall, :d]]]]],

  "a=1,2 rescue 4" =>
    [[:rescue, [:lasgn, :a, [:svalue, [:array, [:lit, 1], [:lit,
     2]]]], [:resbody, nil, [:lit, 4]]]],

  "a=1;\"\#{a}\"" =>
    [[:block, [:lasgn, :a, [:lit, 1]], [:dstr, "", [:evstr, [
    :lvar, :a]]]]],

  "a=1;a" => [[:block, [:lasgn, :a, [:lit, 1]], [:lvar, :a]]],
  "a=1;f{|a,b| a+b}" =>
    [[:block, [:lasgn, :a, [:lit, 1]], [:iter, [:fcall, :f], [
    :masgn, [:array, [:lasgn, :a], [:dasgn_curr, :b]], nil, nil], [
    :call, [:lvar, :a], :+, [:array, [:dvar, :b]]]]]],

  "a=1;f{|a| a}" =>
    [[:block, [:lasgn, :a, [:lit, 1]], [:iter, [:fcall, :f], [
    :lasgn, :a], [:lvar, :a]]]],

  "a===b" => [[:call, [:vcall, :a], :===, [:array, [:vcall, :b]]]],
  "a==b" => [[:call, [:vcall, :a], :==, [:array, [:vcall, :b]]]],
  "a=Array; module a::Foo include a; 55 end" =>
    [[:block, [:lasgn, :a, [:const, :Array]], [:module, [:colon2, [
    :lvar, :a], :Foo], [:scope, [:block, [:fcall, :include, [
    :array, [:vcall, :a]]], [:lit, 55]]]]]],

  "a=Array; module a::Foo include a; a end" =>
    [[:block, [:lasgn, :a, [:const, :Array]], [:module, [:colon2, [
    :lvar, :a], :Foo], [:scope, [:block, [:fcall, :include, [
    :array, [:vcall, :a]]], [:vcall, :a]]]]]],

  "a=Array; module a::Foo; 55 end" =>
    [[:block, [:lasgn, :a, [:const, :Array]], [:module, [:colon2, [
    :lvar, :a], :Foo], [:scope, [:lit, 55]]]]],

  "a=[]; a [42] = 24" =>
    [[:block, [:lasgn, :a, [:zarray]], [:attrasgn, [:lvar, :a],
     :[]=, [:array, [:lit, 42], [:lit, 24]]]]],

  "a=a do end" => [[:lasgn, :a, [:iter, [:fcall, :a], nil]]],
  "a=a i? -R" =>
    [[:lasgn, :a, [:fcall, :a, [:array, [:call, [:fcall, :i?], :-, [
    :array, [:const, :R]]]]]]],

  "a=b do end" => [[:lasgn, :a, [:iter, [:fcall, :b], nil]]],
  "a=b rescue c" =>
    [[:lasgn, :a, [:rescue, [:vcall, :b], [:resbody, nil, [
    :vcall, :c]]]]],

  "a=b rescue c rescue d" =>
    [[:rescue, [:lasgn, :a, [:rescue, [:vcall, :b], [:resbody, nil, [
    :vcall, :c]]]], [:resbody, nil, [:vcall, :d]]]],

  "a=b+=1" =>
    [[:lasgn, :a, [:lasgn, :b, [:call, [:lvar, :b], :+, [:array,
     [:lit, 1]]]]]],

  "a=b,d rescue c" =>
    [[:rescue, [:lasgn, :a, [:svalue, [:array, [:vcall, :b], [
    :vcall, :d]]]], [:resbody, nil, [:vcall, :c]]]],

  "a=b=1" => [[:lasgn, :a, [:lasgn, :b, [:lit, 1]]]],
  "a=b=1,d=2" =>
    [[:lasgn, :a, [:svalue, [:array, [:lasgn, :b, [:lit, 1]], [
    :lasgn, :d, [:lit, 2]]]]]],

  "a=b=c=1;a ?b:c" =>
    [[:block, [:lasgn, :a, [:lasgn, :b, [:lasgn, :c, [:lit, 1]]]
    ], [:if, [:lvar, :a], [:lvar, :b], [:lvar, :c]]]],

  "a=b=c=d=f rescue g" =>
    [[:lasgn, :a, [:lasgn, :b, [:lasgn, :c, [:lasgn, :d, [:rescue, [
    :vcall, :f], [:resbody, nil, [:vcall, :g]]]]]]]],

  "a=b=d rescue c" =>
    [[:lasgn, :a, [:lasgn, :b, [:rescue, [:vcall, :d], [:resbody,
     nil, [:vcall, :c]]]]]],

  "a=begin b end rescue c" =>
    [[:lasgn, :a, [:rescue, [:begin, [:vcall, :b]], [:resbody, nil, [
    :vcall, :c]]]]],

  "a=begin b; rescue c; d end rescue e" =>
    [[:lasgn, :a, [:rescue, [:begin, [:rescue, [:vcall, :b], [
    :resbody, [:array, [:vcall, :c]], [:vcall, :d]]]], [:resbody,
     nil, [:vcall, :e]]]]],

  "a=begin end rescue b" =>
    [[:lasgn, :a, [:rescue, [:nil], [:resbody, nil, [:vcall, :b]
    ]]]],

  "a=p,z=c rescue 0" =>
    [[:lasgn, :a, [:svalue, [:array, [:vcall, :p], [:lasgn, :z, [
    :rescue, [:vcall, :c], [:resbody, nil, [:lit, 0]]]]]]]],

  "a=~b" => [[:call, [:vcall, :a], :=~, [:array, [:vcall, :b]]]],
  "a>=b" => [[:call, [:vcall, :a], :>=, [:array, [:vcall, :b]]]],
  "a>>b" => [[:call, [:vcall, :a], :>>, [:array, [:vcall, :b]]]],
  "a>b" => [[:call, [:vcall, :a], :>, [:array, [:vcall, :b]]]],
  "a[*b]" => [[:call, [:vcall, :a], :[], [:splat, [:vcall, :b]]]],
  "a[*b]+=c" =>
    [[:op_asgn1, [:vcall, :a], [:splat, [:vcall, :b]], :+, [
    :vcall, :c]]],

  "a[*b],z=c" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :a], :[]=, [:splat, [
    :vcall, :b]]], [:lasgn, :z]], nil, [:to_ary, [:vcall, :c]]]],

  "a[*b],z=c,y" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :a], :[]=, [:splat, [
    :vcall, :b]]], [:lasgn, :z]], nil, [:array, [:vcall, :c], [
    :vcall, :y]]]],

  "a[*b]=a=b,c=d" =>
    [[:attrasgn, [:vcall, :a], :[]=, [:argspush, [:splat, [
    :vcall, :b]], [:svalue, [:array, [:lasgn, :a, [:vcall, :b]],
     [:lasgn, :c, [:vcall, :d]]]]]]],

  "a[*b]=c" =>
    [[:attrasgn, [:vcall, :a], :[]=, [:argspush, [:splat, [
    :vcall, :b]], [:vcall, :c]]]],

  "a[*b]||=c" =>
    [[:op_asgn1, [:vcall, :a], [:splat, [:vcall, :b]], :"||", [
    :vcall, :c]]],

  "a[-b]" =>
    [[:call, [:vcall, :a], :[], [:array, [:call, [:vcall, :b], :-@]
    ]]],

  "a[3, 2] = 'a', 'b'" =>
    [[:attrasgn, [:vcall, :a], :[]=, [:array, [:lit, 3], [:lit,
     2], [:svalue, [:array, [:str, "a"], [:str, "b"]]]]]],

  "a[b=>c,*d]" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\na[b=>c,*d]\n        ^"),
  "a[b=>c,*d]+=e" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\na[b=>c,*d]+=e\n        ^"),
  "a[b=>c,*d],z=e" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\na[b=>c,*d],z=e\n        ^"),
  "a[b=>c,*d],z=e,y" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\na[b=>c,*d],z=e,y\n        ^"),
  "a[b=>c,*d]=e" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\na[b=>c,*d]=e\n        ^"),
  "a[b=>c,*d]||=e" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\na[b=>c,*d]||=e\n        ^"),
  "a[b=>c]" =>
    [[:call, [:vcall, :a], :[], [:array, [:hash, [:vcall, :b], [
    :vcall, :c]]]]],

  "a[b=>c]+=d" =>
    [[:op_asgn1, [:vcall, :a], [:array, [:hash, [:vcall, :b], [
    :vcall, :c]]], :+, [:vcall, :d]]],

  "a[b=>c],z=d" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :a], :[]=, [:array, [
    :hash, [:vcall, :b], [:vcall, :c]]]], [:lasgn, :z]], nil, [
    :to_ary, [:vcall, :d]]]],

  "a[b=>c],z=d,y" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :a], :[]=, [:array, [
    :hash, [:vcall, :b], [:vcall, :c]]]], [:lasgn, :z]], nil, [
    :array, [:vcall, :d], [:vcall, :y]]]],

  "a[b=>c]=d" =>
    [[:attrasgn, [:vcall, :a], :[]=, [:array, [:hash, [:vcall, :b], [
    :vcall, :c]], [:vcall, :d]]]],

  "a[b=>c]||=d" =>
    [[:op_asgn1, [:vcall, :a], [:array, [:hash, [:vcall, :b], [
    :vcall, :c]]], :"||", [:vcall, :d]]],

  "a[b]||=c" =>
    [[:op_asgn1, [:vcall, :a], [:array, [:vcall, :b]], :"||", [
    :vcall, :c]]],

  "a[d]=b rescue c" =>
    [[:attrasgn, [:vcall, :a], :[]=, [:array, [:vcall, :d], [
    :rescue, [:vcall, :b], [:resbody, nil, [:vcall, :c]]]]]],

  "a[y,z,*b]" =>
    [[:call, [:vcall, :a], :[], [:argscat, [:array, [:vcall, :y], [
    :vcall, :z]], [:vcall, :b]]]],

  "a[y,z,*b]+=c" =>
    [[:op_asgn1, [:vcall, :a], [:argscat, [:array, [:vcall, :y],
     [:vcall, :z]], [:vcall, :b]], :+, [:vcall, :c]]],

  "a[y,z,*b],z=c" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :a], :[]=, [:argscat, [
    :array, [:vcall, :y], [:vcall, :z]], [:vcall, :b]]], [:lasgn,
     :z]], nil, [:to_ary, [:vcall, :c]]]],

  "a[y,z,*b],z=c,y" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :a], :[]=, [:argscat, [
    :array, [:vcall, :y], [:vcall, :z]], [:vcall, :b]]], [:lasgn,
     :z]], nil, [:array, [:vcall, :c], [:vcall, :y]]]],

  "a[y,z,*b]=c" =>
    [[:attrasgn, [:vcall, :a], :[]=, [:argspush, [:argscat, [
    :array, [:vcall, :y], [:vcall, :z]], [:vcall, :b]], [:vcall,
     :c]]]],

  "a[y,z,*b]||=c" =>
    [[:op_asgn1, [:vcall, :a], [:argscat, [:array, [:vcall, :y],
     [:vcall, :z]], [:vcall, :b]], :"||", [:vcall, :c]]],

  "a[y,z,b=>c,*d]" => SyntaxError.new("(string):1: syntax error, unexpected tASSOC, expecting ']'\na[y,z,b=>c,*d]\n         ^\n(string):1: syntax error, unexpected ']', expecting '='\na[y,z,b=>c,*d]\n              ^"),
  "a[y,z,b=>c,*d]=e" => SyntaxError.new("(string):1: syntax error, unexpected tASSOC, expecting ']'\na[y,z,b=>c,*d]=e\n         ^\n(string):1: syntax error, unexpected ']', expecting '='\na[y,z,b=>c,*d]=e\n              ^"),
  "a[y,z,b=>c]" => SyntaxError.new("(string):1: syntax error, unexpected tASSOC, expecting ']'\na[y,z,b=>c]\n         ^"),
  "a[y,z,b=>c]=d" => SyntaxError.new("(string):1: syntax error, unexpected tASSOC, expecting ']'\na[y,z,b=>c]=d\n         ^"),
  "a[z,*b]" =>
    [[:call, [:vcall, :a], :[], [:argscat, [:array, [:vcall, :z]
    ], [:vcall, :b]]]],

  "a[z,*b]+=c" =>
    [[:op_asgn1, [:vcall, :a], [:argscat, [:array, [:vcall, :z]
    ], [:vcall, :b]], :+, [:vcall, :c]]],

  "a[z,*b],z=c" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :a], :[]=, [:argscat, [
    :array, [:vcall, :z]], [:vcall, :b]]], [:lasgn, :z]], nil, [
    :to_ary, [:vcall, :c]]]],

  "a[z,*b],z=c,y" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :a], :[]=, [:argscat, [
    :array, [:vcall, :z]], [:vcall, :b]]], [:lasgn, :z]], nil, [
    :array, [:vcall, :c], [:vcall, :y]]]],

  "a[z,*b]=c" =>
    [[:attrasgn, [:vcall, :a], :[]=, [:argspush, [:argscat, [
    :array, [:vcall, :z]], [:vcall, :b]], [:vcall, :c]]]],

  "a[z,*b]||=c" =>
    [[:op_asgn1, [:vcall, :a], [:argscat, [:array, [:vcall, :z]
    ], [:vcall, :b]], :"||", [:vcall, :c]]],

  "a[z,b=>c,*d]" => SyntaxError.new("(string):1: syntax error, unexpected tASSOC, expecting ']'\na[z,b=>c,*d]\n       ^\n(string):1: syntax error, unexpected ']', expecting '='\na[z,b=>c,*d]\n            ^"),
  "a[z,b=>c,*d]=e" => SyntaxError.new("(string):1: syntax error, unexpected tASSOC, expecting ']'\na[z,b=>c,*d]=e\n       ^\n(string):1: syntax error, unexpected ']', expecting '='\na[z,b=>c,*d]=e\n            ^"),
  "a[z,b=>c]" => SyntaxError.new("(string):1: syntax error, unexpected tASSOC, expecting ']'\na[z,b=>c]\n       ^"),
  "a[z,b=>c]=d" => SyntaxError.new("(string):1: syntax error, unexpected tASSOC, expecting ']'\na[z,b=>c]=d\n       ^"),
  "a^b" => [[:call, [:vcall, :a], :^, [:array, [:vcall, :b]]]],
  "aa,(a,)=1" =>
    [[:masgn, [:array, [:lasgn, :aa], [:masgn, [:array, [:lasgn,
     :a]], nil, nil]], nil, [:to_ary, [:lit, 1]]]],

  "aa,(a,*) =1" =>
    [[:masgn, [:array, [:lasgn, :aa], [:masgn, [:array, [:lasgn,
     :a]], [:splat], nil]], nil, [:to_ary, [:lit, 1]]]],

  "aa,(a,b,)=1" =>
    [[:masgn, [:array, [:lasgn, :aa], [:masgn, [:array, [:lasgn,
     :a], [:lasgn, :b]], nil, nil]], nil, [:to_ary, [:lit, 1]]]],

  "aa,(a,b,*) =1" =>
    [[:masgn, [:array, [:lasgn, :aa], [:masgn, [:array, [:lasgn,
     :a], [:lasgn, :b]], [:splat], nil]], nil, [:to_ary, [:lit,
     1]]]],

  "\
aaa=<<whatnot; p \"\#{'uh,yeah'
gonna take it down, to the nitty-grit
gonna tell you mother-fuckers why you ain't shit
cause suckers like you just make me strong
you been pumpin' that bullshit all day long
whatnot
}\"
p aaa\

" =>
    [[:block, [:lasgn, :aaa, [
    :str, "gonna take it down, to the nitty-grit\ngonna tell you mother-fuckers why you ain't shit\ncause suckers like you just make me strong\nyou been pumpin' that bullshit all day long\n"]], [
    :fcall, :p, [:array, [:str, "uh,yeah"]]], [:fcall, :p, [
    :array, [:lvar, :aaa]]]]],

  "alias $ERROR_INFO $!" => [[:valias, :$ERROR_INFO, :$!]],
  "alias $ERROR_INFO :$!" => SyntaxError.new("(string):1: syntax error, unexpected ':', expecting tGVAR or tNTH_REF or tBACK_REF\nalias $ERROR_INFO :$!\n                   ^"),
  "alias * com_disk" => [[:alias, [:lit, :*], [:lit, :com_disk]]],
  "alias - com_disk" => [[:alias, [:lit, :-], [:lit, :com_disk]]],
  "\
alias :\"
__END__
\#{bar}\" :\"baz\#{quux}\"" =>
    [[:alias, [:dsym, "\n__END__\n", [:evstr, [:vcall, :bar]]], [
    :dsym, "baz", [:evstr, [:vcall, :quux]]]]],

  "alias :\"foo\" bar\#{mdfgnxc}" =>
    [[:alias, [:lit, :foo], [:lit, :bar]]],

  "alias :\"foo\#{bar}\" :\"baz\#{quux}\"" =>
    [[:alias, [:dsym, "foo", [:evstr, [:vcall, :bar]]], [:dsym,
     "baz", [:evstr, [:vcall, :quux]]]]],

  "alias :\"foo\#{dfgdfg}\" bar" =>
    [[:alias, [:dsym, "foo", [:evstr, [:vcall, :dfgdfg]]], [:lit,
     :bar]]],

  "alias :\"foo\#{dfgdfg}\" bar\#{fgsdg}" =>
    [[:alias, [:dsym, "foo", [:evstr, [:vcall, :dfgdfg]]], [:lit,
     :bar]]],

  "alias :$ERROR_INFO $!" => SyntaxError.new("(string):1: syntax error, unexpected tGVAR\nalias :$ERROR_INFO $!\n                     ^"),
  "alias :$ERROR_INFO :$!" =>
    [[:alias, [:lit, :$ERROR_INFO], [:lit, :$!]]],

  "alias :'foo' bar" => [[:alias, [:lit, :foo], [:lit, :bar]]],
  "alias :a :b" => [[:alias, [:lit, :a], [:lit, :b]]],
  "alias :~@ non" => [[:alias, [:lit, :~], [:lit, :non]]],
  "alias a b" => [[:alias, [:lit, :a], [:lit, :b]]],
  "alias non :~@" => [[:alias, [:lit, :non], [:lit, :~]]],
  "alias non ~@" => [[:alias, [:lit, :non], [:lit, :~]]],
  "alias ~@ non" => [[:alias, [:lit, :~], [:lit, :non]]],
  "\
alias
q p" =>
    [[:alias, [:lit, :q], [:lit, :p]]],

  "\
alias
q
p" =>
    [[:alias, [:lit, :q], [:lit, :p]]],

  "assert_equal \"\\177\\377\\377\\377\", [-2**31-1].pack(\"N\")\
" =>
    [[:fcall, :assert_equal, [:array, [:str, "\x7F\xFF\xFF\xFF"], [
    :call, [:array, [:call, [:call, [:call, [:lit, 2], :**, [
    :array, [:lit, 31]]], :-@], :-, [:array, [:lit, 1]]]], :pack, [
    :array, [:str, "N"]]]]]],

  "assert_equal(\"22aacd445566\", \"\#{22}aa\" \"cd\#{44}\"\
 \"55\" \"\#{66}\")" =>
    [[:fcall, :assert_equal, [:array, [:str, "22aacd445566"], [
    :dstr, "", [:evstr, [:lit, 22]], [:str, "aa"], [:str, "cd"],
     [:evstr, [:lit, 44]], [:str, "55"], [:evstr, [:lit, 66]]]]]
    ],

  "attribute :invisible do [] end" =>
    [[:iter, [:fcall, :attribute, [:array, [:lit, :invisible]]],
     nil, [:zarray]]],

  "a{b}" => [[:iter, [:fcall, :a], nil, [:vcall, :b]]],
  "a{}" => [[:iter, [:fcall, :a], nil]],
  "a|b" => [[:call, [:vcall, :a], :|, [:array, [:vcall, :b]]]],
  "a||(b||c)" =>
    [[:or, [:vcall, :a], [:or, [:vcall, :b], [:vcall, :c]]]],

  "a||=b" =>
    [[:op_asgn_or, [:lvar, :a], [:lasgn, :a, [:vcall, :b]]]],

  "a||b" => [[:or, [:vcall, :a], [:vcall, :b]]],
  "a||b&&c" =>
    [[:or, [:vcall, :a], [:and, [:vcall, :b], [:vcall, :c]]]],

  "a||b||c" =>
    [[:or, [:vcall, :a], [:or, [:vcall, :b], [:vcall, :c]]]],

  "b && p (1).m" =>
    [[:and, [:vcall, :b], [:call, [:fcall, :p, [:array, [:lit, 1]
    ]], :m]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "b ? p (1).m : g" =>
    [[:if, [:vcall, :b], [:call, [:fcall, :p, [:array, [:lit, 1]
    ]], :m], [:vcall, :g]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "b a=a do end" =>
    [[:iter, [:fcall, :b, [:array, [:lasgn, :a, [:lvar, :a]]]],
     nil]],

  "b and p (1).m" =>
    [[:and, [:vcall, :b], [:call, [:fcall, :p, [:array, [:lit, 1]
    ]], :m]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "b do * @com_disk end" => SyntaxError.new("(string):1: syntax error, unexpected kEND, expecting '='\nb do * @com_disk end\n                    ^"),
  "b do - @com_disk end" =>
    [[:iter, [:fcall, :b], nil, [:call, [:ivar, :@com_disk], :-@]
    ]],

  "b do c end" => [[:iter, [:fcall, :b], nil, [:vcall, :c]]],
  "b if /vt100/ =~ line" =>
    [[:if, [:match2, [:lit, /vt100/], [:vcall, :line]], [:vcall,
     :b], nil]],

  "b or p (1).m" =>
    [[:or, [:vcall, :b], [:call, [:fcall, :p, [:array, [:lit, 1]
    ]], :m]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "b || p (1).m" =>
    [[:or, [:vcall, :b], [:call, [:fcall, :p, [:array, [:lit, 1]
    ]], :m]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "b,(c,*d),*e=z" =>
    [[:masgn, [:array, [:lasgn, :b], [:masgn, [:array, [:lasgn,
     :c]], [:lasgn, :d], nil]], [:lasgn, :e], [:to_ary, [:vcall,
     :z]]]],

  "b,(c,d),*e=z" =>
    [[:masgn, [:array, [:lasgn, :b], [:masgn, [:array, [:lasgn,
     :c], [:lasgn, :d]], nil, nil]], [:lasgn, :e], [:to_ary, [
    :vcall, :z]]]],

  "b,(c,d),e=z" =>
    [[:masgn, [:array, [:lasgn, :b], [:masgn, [:array, [:lasgn,
     :c], [:lasgn, :d]], nil, nil], [:lasgn, :e]], nil, [:to_ary, [
    :vcall, :z]]]],

  "b,(c.f,*d),*e=z" =>
    [[:masgn, [:array, [:lasgn, :b], [:masgn, [:array, [:attrasgn, [
    :vcall, :c], :f=]], [:lasgn, :d], nil]], [:lasgn, :e], [
    :to_ary, [:vcall, :z]]]],

  "b,(c.f,g[h],*d),*e=z" =>
    [[:masgn, [:array, [:lasgn, :b], [:masgn, [:array, [:attrasgn, [
    :vcall, :c], :f=], [:attrasgn, [:vcall, :g], :[]=, [:array, [
    :vcall, :h]]]], [:lasgn, :d], nil]], [:lasgn, :e], [:to_ary,
     [:vcall, :z]]]],

  "b,(c[f],*d),*e=z" =>
    [[:masgn, [:array, [:lasgn, :b], [:masgn, [:array, [:attrasgn, [
    :vcall, :c], :[]=, [:array, [:vcall, :f]]]], [:lasgn, :d], nil]
    ], [:lasgn, :e], [:to_ary, [:vcall, :z]]]],

  "b,c,*d=z" =>
    [[:masgn, [:array, [:lasgn, :b], [:lasgn, :c]], [:lasgn, :d], [
    :to_ary, [:vcall, :z]]]],

  "b,c=z" =>
    [[:masgn, [:array, [:lasgn, :b], [:lasgn, :c]], nil, [:to_ary, [
    :vcall, :z]]]],

  "b.c,d[e],*f=z" =>
    [[:masgn, [:array, [:attrasgn, [:vcall, :b], :c=], [:attrasgn, [
    :vcall, :d], :[]=, [:array, [:vcall, :e]]]], [:lasgn, :f], [
    :to_ary, [:vcall, :z]]]],

  "b.c=1" => [[:attrasgn, [:vcall, :b], :c=, [:array, [:lit, 1]]]],
  "b.c=d" =>
    [[:attrasgn, [:vcall, :b], :c=, [:array, [:vcall, :d]]]],

  "b::c=d" =>
    [[:attrasgn, [:vcall, :b], :c=, [:array, [:vcall, :d]]]],

  "b=1;a(b)" =>
    [[:block, [:lasgn, :b, [:lit, 1]], [:fcall, :a, [:array, [
    :lvar, :b]]]]],

  "b=1;a=b do end" =>
    [[:block, [:lasgn, :b, [:lit, 1]], [:lasgn, :a, [:iter, [
    :fcall, :b], nil]]]],

  "b=1;b p %s[jim];" =>
    [[:block, [:lasgn, :b, [:lit, 1]], [:fcall, :b, [:array, [
    :call, [:vcall, :p], :%, [:array, [:call, [:vcall, :s], :[],
     [:array, [:vcall, :jim]]]]]]]]],

  "b[3]" => [[:call, [:vcall, :b], :[], [:array, [:lit, 3]]]],
  "b[]" => [[:call, [:vcall, :b], :[]]],
  "\
before_time = Time.now
ARGV.each do |fname|
  if fname == '-v'
    verbose = true
    next
  end
  rss = nil
  f = File.new(fname).read
  begin
    ## do validate parse
    rss = RSS::Parser.parse(f)
  rescue RSS::InvalidRSSError
    error($!) if verbose
    ## do non validate parse for invalid RSS 1.0
    begin
      rss = RSS::Parser.parse(f, false)
    rescue RSS::Error
      ## invalid RSS.
      error($!) if verbose
    end
  rescue RSS::Error
    error($!) if verbose
  end
end" =>
    [[:block, [:lasgn, :before_time, [:call, [:const, :Time], :now]
    ], [:iter, [:call, [:const, :ARGV], :each], [:dasgn_curr, :fname], [
    :block, [:if, [:call, [:dvar, :fname], :==, [:array, [:str,
     "-v"]]], [:block, [:dasgn_curr, :verbose, [:true]], [:next]
    ], nil], [:dasgn_curr, :rss, [:nil]], [:dasgn_curr, :f, [
    :call, [:call, [:const, :File], :new, [:array, [:dvar, :fname]
    ]], :read]], [:rescue, [:dasgn_curr, :rss, [:call, [:colon2,
     [:const, :RSS], :Parser], :parse, [:array, [:dvar, :f]]]], [
    :resbody, [:array, [:colon2, [:const, :RSS], :InvalidRSSError]
    ], [:block, [:if, [:dvar, :verbose], [:fcall, :error, [:array, [
    :gvar, :$!]]], nil], [:rescue, [:dasgn_curr, :rss, [:call, [
    :colon2, [:const, :RSS], :Parser], :parse, [:array, [:dvar,
     :f], [:false]]]], [:resbody, [:array, [:colon2, [:const,
     :RSS], :Error]], [:if, [:dvar, :verbose], [:fcall, :error, [
    :array, [:gvar, :$!]]], nil]]]], [:resbody, [:array, [:colon2, [
    :const, :RSS], :Error]], [:if, [:dvar, :verbose], [:fcall,
     :error, [:array, [:gvar, :$!]]], nil]]]]]]]],

  "begin * @com_disk;end" => SyntaxError.new("(string):1: syntax error, unexpected ';', expecting '='\nbegin * @com_disk;end\n                  ^"),
  "begin - @com_disk;end" => [[:call, [:ivar, :@com_disk], :-@]],
  "begin a end rescue b0" =>
    [[:rescue, [:vcall, :a], [:resbody, nil, [:vcall, :b0]]]],

  "begin a end until !b" =>
    [[:while, [:vcall, :b], [:vcall, :a], false]],

  "begin a end until b" =>
    [[:until, [:vcall, :b], [:vcall, :a], false]],

  "begin a end until b!=c" =>
    [[:while, [:call, [:vcall, :b], :==, [:array, [:vcall, :c]]
    ], [:vcall, :a], false]],

  "begin a end until b!~c" =>
    [[:until, [:fcall, :b!, [:array, [:call, [:vcall, :c], :~]]], [
    :vcall, :a], false]],

  "begin a end until b==c" =>
    [[:until, [:call, [:vcall, :b], :==, [:array, [:vcall, :c]]
    ], [:vcall, :a], false]],

  "begin a end until b=~c" =>
    [[:until, [:call, [:vcall, :b], :=~, [:array, [:vcall, :c]]
    ], [:vcall, :a], false]],

  "begin a end until not b" =>
    [[:while, [:vcall, :b], [:vcall, :a], false]],

  "begin a end while !b" =>
    [[:until, [:vcall, :b], [:vcall, :a], false]],

  "begin a end while b" =>
    [[:while, [:vcall, :b], [:vcall, :a], false]],

  "begin a end while b!=c" =>
    [[:until, [:call, [:vcall, :b], :==, [:array, [:vcall, :c]]
    ], [:vcall, :a], false]],

  "begin a end while b!~c" =>
    [[:while, [:fcall, :b!, [:array, [:call, [:vcall, :c], :~]]], [
    :vcall, :a], false]],

  "begin a end while b==c" =>
    [[:while, [:call, [:vcall, :b], :==, [:array, [:vcall, :c]]
    ], [:vcall, :a], false]],

  "begin a end while b=~c" =>
    [[:while, [:call, [:vcall, :b], :=~, [:array, [:vcall, :c]]
    ], [:vcall, :a], false]],

  "begin a end while not b" =>
    [[:until, [:vcall, :b], [:vcall, :a], false]],

  "begin a; else b end while c" =>
    [[:while, [:vcall, :c], [:block, [:vcall, :a], [:vcall, :b]
    ], false],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "begin a; ensure * @com_disk; end" => SyntaxError.new("(string):1: syntax error, unexpected ';', expecting '='\nbegin a; ensure * @com_disk; end\n                            ^"),
  "begin a; ensure - @com_disk; end" =>
    [[:ensure, [:vcall, :a], [:call, [:ivar, :@com_disk], :-@]]],

  "begin a; ensure b end while c" =>
    [[:while, [:vcall, :c], [:ensure, [:vcall, :a], [:vcall, :b]
    ], false]],

  "begin a; rescue b,c,f; d end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:vcall, :b], [
    :vcall, :c], [:vcall, :f]], [:vcall, :d]]]],

  "begin a; rescue b,f: c end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:vcall, :b], [
    :vcall, :f]], [:vcall, :c]]]],

  "begin a; rescue b,f=>g: c end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:vcall, :b], [
    :vcall, :f]], [:block, [:lasgn, :g, [:gvar, :$!]], [:vcall,
     :c]]]]],

  "begin a; rescue b; c end while d" =>
    [[:while, [:vcall, :d], [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]], [:vcall, :c]]], false]],

  "begin a; rescue b=>e; @c=e.f; end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:vcall, :b]], [
    :block, [:lasgn, :e, [:gvar, :$!]], [:iasgn, :@c, [:call, [
    :lvar, :e], :f]]]]]],

  "begin a;b end while c" =>
    [[:while, [:vcall, :c], [:block, [:vcall, :a], [:vcall, :b]
    ], false]],

  "begin a;b; else c end while d" =>
    [[:while, [:vcall, :d], [:block, [:vcall, :a], [:vcall, :b],
     [:vcall, :c]], false],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "begin a;b; ensure c end while d" =>
    [[:while, [:vcall, :d], [:ensure, [:block, [:vcall, :a], [
    :vcall, :b]], [:vcall, :c]], false]],

  "begin a;b; rescue c; d end while e" =>
    [[:while, [:vcall, :e], [:rescue, [:block, [:vcall, :a], [
    :vcall, :b]], [:resbody, [:array, [:vcall, :c]], [:vcall, :d]
    ]], false]],

  "begin a;g; rescue b,c,f; d end" =>
    [[:rescue, [:block, [:vcall, :a], [:vcall, :g]], [:resbody, [
    :array, [:vcall, :b], [:vcall, :c], [:vcall, :f]], [:vcall,
     :d]]]],

  "begin a;h; rescue b,f: c end" =>
    [[:rescue, [:block, [:vcall, :a], [:vcall, :h]], [:resbody, [
    :array, [:vcall, :b], [:vcall, :f]], [:vcall, :c]]]],

  "begin a;h; rescue b,f=>g: c end" =>
    [[:rescue, [:block, [:vcall, :a], [:vcall, :h]], [:resbody, [
    :array, [:vcall, :b], [:vcall, :f]], [:block, [:lasgn, :g, [
    :gvar, :$!]], [:vcall, :c]]]]],

  "begin begin; a; rescue b; end end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:vcall, :b]]]]
    ],

  "begin begin; ync; p1; end;rr end" =>
    [[:block, [:vcall, :ync], [:vcall, :p1], [:vcall, :rr]]],

  "begin else a end while b" =>
    [[:while, [:vcall, :b], [:vcall, :a], false],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "begin else ensure end" => [[:ensure, [:nil]]],
  "begin end" => [[:nil]],
  "begin end until a" => [[:until, [:vcall, :a], [:nil], true]],
  "begin end while b" => [[:while, [:vcall, :b], [:nil], true]],
  "begin ensure a end while b" =>
    [[:while, [:vcall, :b], [:ensure, [:vcall, :a]], false]],

  "begin k;l;m rescue n; rescue o,p,q=>r: s; rescue t; u; rescue;\
 v else w ensure x end" =>
    [[:ensure, [:rescue, [:block, [:vcall, :k], [:vcall, :l], [
    :rescue, [:vcall, :m], [:resbody, nil, [:vcall, :n]]]], [
    :resbody, [:array, [:vcall, :o], [:vcall, :p], [:vcall, :q]
    ], [:block, [:lasgn, :r, [:gvar, :$!]], [:vcall, :s]], [
    :resbody, [:array, [:vcall, :t]], [:vcall, :u], [:resbody, nil, [
    :vcall, :v]]]], [:vcall, :w]], [:vcall, :x]]],

  "begin r;rescue *re;r;end" =>
    [[:rescue, [:vcall, :r], [:resbody, [:splat, [:vcall, :re]],
     [:vcall, :r]]]],

  "begin rescue ; end" => [[:rescue, [:resbody, nil]]],
  "begin rescue B; rescue ; end" =>
    [[:rescue, [:resbody, [:array, [:const, :B]], [:resbody, nil]
    ]]],

  "begin rescue a; b end while c" =>
    [[:while, [:vcall, :c], [:rescue, [:resbody, [:array, [
    :vcall, :a]], [:vcall, :b]]], false]],

  "begin undef a,b;rr end" =>
    [[:block, [:undef, [:lit, :a]], [:undef, [:lit, :b]], [
    :vcall, :rr]]],

  "begin v; else return ([vUew]).e  end" =>
    [[:block, [:vcall, :v], [:return, [:call, [:array, [:vcall,
     :vUew]], :e]]],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "begin z; rescue a=b=1,d=2; yy end" =>
    [[:rescue, [:vcall, :z], [:resbody, [:array, [:lasgn, :a, [
    :lasgn, :b, [:lit, 1]]], [:lasgn, :d, [:lit, 2]]], [:vcall,
     :yy]]]],

  "begin; a; b; else f ensure; e; end" =>
    [[:ensure, [:block, [:vcall, :a], [:vcall, :b], [:vcall, :f]
    ], [:vcall, :e]],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "begin; a; b; else f; g ensure; e; end" =>
    [[:ensure, [:block, [:vcall, :a], [:vcall, :b], [:vcall, :f], [
    :vcall, :g]], [:vcall, :e]],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "begin; a; else f; ensure; e; end" =>
    [[:ensure, [:block, [:vcall, :a], [:vcall, :f]], [:vcall, :e]
    ],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "begin; a; else f; g ensure; e; end" =>
    [[:ensure, [:block, [:vcall, :a], [:vcall, :f], [:vcall, :g]
    ], [:vcall, :e]],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "begin; a; rescue =>c; end" =>
    [[:rescue, [:vcall, :a], [:resbody, nil, [:lasgn, :c, [:gvar,
     :$!]]]]],

  "begin; a; rescue B; d; e; end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:const, :B]], [
    :block, [:vcall, :d], [:vcall, :e]]]]],

  "begin; a; rescue B; d; else f; end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:const, :B]], [
    :vcall, :d]], [:vcall, :f]]],

  "begin; a; rescue B; d; else f; ensure; e; end" =>
    [[:ensure, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :const, :B]], [:vcall, :d]], [:vcall, :f]], [:vcall, :e]]],

  "begin; a; rescue B; d; end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:const, :B]], [
    :vcall, :d]]]],

  "begin; a; rescue B; end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:const, :B]]]]
    ],

  "begin; a; rescue B=>c; d; e; end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:const, :B]], [
    :block, [:lasgn, :c, [:gvar, :$!]], [:vcall, :d], [:vcall, :e]
    ]]]],

  "begin; a; rescue B=>c; d; else f; ensure; e; end" =>
    [[:ensure, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :const, :B]], [:block, [:lasgn, :c, [:gvar, :$!]], [:vcall,
     :d]]], [:vcall, :f]], [:vcall, :e]]],

  "begin; a; rescue B=>c; d; end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:const, :B]], [
    :block, [:lasgn, :c, [:gvar, :$!]], [:vcall, :d]]]]],

  "begin; a; rescue B=>c; end" =>
    [[:rescue, [:vcall, :a], [:resbody, [:array, [:const, :B]], [
    :lasgn, :c, [:gvar, :$!]]]]],

  "begin; a; rescue b; end && m" =>
    [[:and, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], [:vcall, :m]]],

  "begin; a; rescue b; end ; m" =>
    [[:block, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]], [:vcall, :m]]],

  "begin; a; rescue b; end ? m : n" =>
    [[:if, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], [:vcall, :m], [:vcall, :n]]],

  "begin; a; rescue b; end and m" =>
    [[:and, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], [:vcall, :m]]],

  "begin; a; rescue b; end if m" =>
    [[:if, [:vcall, :m], [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]], nil]],

  "begin; a; rescue b; end or m" =>
    [[:or, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], [:vcall, :m]]],

  "begin; a; rescue b; end while m" =>
    [[:while, [:vcall, :m], [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]], false]],

  "begin; a; rescue b; end || m" =>
    [[:or, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], [:vcall, :m]]],

  "begin; a; rescue b; end+m" =>
    [[:call, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], :+, [:array, [:vcall, :m]]]],

  "begin; a; rescue b; end.each" =>
    [[:call, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], :each]],

  "begin; a; rescue b; end.m" =>
    [[:call, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], :m]],

  "begin; a; rescue b; end; c" =>
    [[:block, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]], [:vcall, :c]]],

  "begin; a; rescue b; end[m]" =>
    [[:call, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], :[], [:array, [:vcall, :m]]]],

  "begin; a; rescue b; end[m]=n" =>
    [[:attrasgn, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]], :[]=, [:array, [:vcall, :m], [:vcall, :n]]]],

  "begin; a; rescue; end" =>
    [[:rescue, [:vcall, :a], [:resbody, nil]]],

  "begin; else; ensure; end" => [[:ensure, [:nil]]],
  "begin; end" => [[:nil]],
  "begin; g; rescue *a=r; end" =>
    [[:rescue, [:vcall, :g], [:resbody, [:splat, [:lasgn, :a, [
    :vcall, :r]]]]]],

  "begin; m; rescue A=>p (1).metho; end" =>
    [[:rescue, [:vcall, :m], [:resbody, [:array, [:const, :A]], [
    :attrasgn, [:fcall, :p, [:array, [:lit, 1]]], :metho=, [
    :array, [:gvar, :$!]]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "begin; m; rescue begin; a; rescue b; end,o:  end" =>
    [[:rescue, [:vcall, :m], [:resbody, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]], [:vcall, :o]
    ]]]],

  "begin; m; rescue begin; a; rescue b; end:  end" =>
    [[:rescue, [:vcall, :m], [:resbody, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "begin; p (1..10).method(:each); rescue B=>c; end" =>
    [[:rescue, [:fcall, :p, [:array, [:call, [:lit, 1..10], :method, [
    :array, [:lit, :each]]]]], [:resbody, [:array, [:const, :B]
    ], [:lasgn, :c, [:gvar, :$!]]]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "begin; p (1..10).method(:each); rescue b; end.m" =>
    [[:call, [:rescue, [:fcall, :p, [:array, [:call, [:lit, 1..10],
     :method, [:array, [:lit, :each]]]]], [:resbody, [:array, [
    :vcall, :b]]]], :m],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "begin; p rescue;o_chmod rescue nil;end" => SyntaxError.new("(string):1: syntax error, unexpected ';'\nbegin; p rescue;o_chmod rescue nil;end\n                ^"),
  "begin; p; rescue p ().m; end" =>
    [[:rescue, [:vcall, :p], [:resbody, [:array, [:call, [:fcall,
     :p], :m]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "begin; p; rescue p (4).m; end" =>
    [[:rescue, [:vcall, :p], [:resbody, [:array, [:call, [:fcall,
     :p, [:array, [:lit, 4]]], :m]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "begin; p; rescue p (4,5).m; end" =>
    [[:rescue, [:vcall, :p], [:resbody, [:array, [:call, [:fcall,
     :p, [:array, [:lit, 4], [:lit, 5]]], :m]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "begin; p; rescue p(4).m; end" =>
    [[:rescue, [:vcall, :p], [:resbody, [:array, [:call, [:fcall,
     :p, [:array, [:lit, 4]]], :m]]]]],

  "begin; q; rescue a=r,b; end" =>
    [[:rescue, [:vcall, :q], [:resbody, [:array, [:lasgn, :a, [
    :vcall, :r]], [:vcall, :b]]]]],

  "begin; r; t end;q p" =>
    [[:block, [:vcall, :r], [:vcall, :t], [:fcall, :q, [:array, [
    :vcall, :p]]]]],

  "begin; rescue =>e; p end" =>
    [[:rescue, [:resbody, nil, [:block, [:lasgn, :e, [:gvar, :$!]
    ], [:vcall, :p]]]]],

  "begin; rescue A=>$!; end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:gasgn, :$!, [
    :gvar, :$!]]]]],

  "begin; rescue A=>$a; end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:gasgn, :$a, [
    :gvar, :$!]]]]],

  "begin; rescue A=>::B; end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:cdecl, [
    :colon3, :B], [:gvar, :$!]]]]],

  "begin; rescue A=>@@a; end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:cvdecl, :@@a, [
    :gvar, :$!]]]]],

  "begin; rescue A=>@a; end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:iasgn, :@a, [
    :gvar, :$!]]]]],

  "begin; rescue A=>A::B; end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:cdecl, [
    :colon2, [:const, :A], :B], [:gvar, :$!]]]]],

  "begin; rescue A=>A; end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:cdecl, :A, [
    :gvar, :$!]]]]],

  "begin; rescue A=>a.b; end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:attrasgn, [
    :vcall, :a], :b=, [:array, [:gvar, :$!]]]]]],

  "begin; rescue A=>b.c : end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:attrasgn, [
    :vcall, :b], :c=, [:array, [:gvar, :$!]]]]]],

  "begin; rescue A=>b.c then end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:attrasgn, [
    :vcall, :b], :c=, [:array, [:gvar, :$!]]]]]],

  "\
begin; rescue A=>b.c
end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:attrasgn, [
    :vcall, :b], :c=, [:array, [:gvar, :$!]]]]]],

  "begin; rescue A=>p (1).m; end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:attrasgn, [
    :fcall, :p, [:array, [:lit, 1]]], :m=, [:array, [:gvar, :$!]
    ]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "begin; rescue A=>p[4]; end" =>
    [[:rescue, [:resbody, [:array, [:const, :A]], [:attrasgn, [
    :vcall, :p], :[]=, [:array, [:lit, 4], [:gvar, :$!]]]]]],

  "begin; rescue E=>e; p end" =>
    [[:rescue, [:resbody, [:array, [:const, :E]], [:block, [
    :lasgn, :e, [:gvar, :$!]], [:vcall, :p]]]]],

  "begin;mode;rescue;o_chmod rescue nil;end" =>
    [[:rescue, [:vcall, :mode], [:resbody, nil, [:rescue, [
    :vcall, :o_chmod], [:resbody, nil, [:nil]]]]]],

  "\
begin
  1 rescue 2
 rescue; 2
end
\ndef foo
\n  1 rescue 2
 rescue; 2
\nend
\n
class Foo
\n  1 rescue 2
 rescue; 2
\nend
\n
module Moo
\n  1 rescue 2
 rescue; 2
\nend
\n
class <<Foo
\n  1 rescue 2
 rescue; 2
\nend\

" =>
    [[:block, [:rescue, [:rescue, [:lit, 1], [:resbody, nil, [
    :lit, 2]]], [:resbody, nil, [:lit, 2]]], [:defn, :foo, [
    :scope, [:block, [:args], [:rescue, [:rescue, [:lit, 1], [
    :resbody, nil, [:lit, 2]]], [:resbody, nil, [:lit, 2]]]]]], [
    :class, :Foo, nil, [:scope, [:rescue, [:rescue, [:lit, 1], [
    :resbody, nil, [:lit, 2]]], [:resbody, nil, [:lit, 2]]]]], [
    :module, :Moo, [:scope, [:rescue, [:rescue, [:lit, 1], [
    :resbody, nil, [:lit, 2]]], [:resbody, nil, [:lit, 2]]]]], [
    :sclass, [:const, :Foo], [:scope, [:rescue, [:rescue, [:lit,
     1], [:resbody, nil, [:lit, 2]]], [:resbody, nil, [:lit, 2]]
    ]]]]],

  "break" => [[:break]],
  "break % 1 " => [[:break, [:str, "1"]]],
  "break * @com_disk" =>
    [[:break, [:svalue, [:splat, [:ivar, :@com_disk]]]]],

  "break *a " => [[:break, [:svalue, [:splat, [:vcall, :a]]]]],
  "break *c" => [[:break, [:svalue, [:splat, [:vcall, :c]]]]],
  "break - @com_disk" =>
    [[:break, [:call, [:ivar, :@com_disk], :-@]]],

  "break / 1 /" => [[:break, [:lit, / 1 /]]],
  "break 1" => [[:break, [:lit, 1]]],
  "break 1,*b" =>
    [[:break, [:argscat, [:array, [:lit, 1]], [:vcall, :b]]]],

  "break 1,2" => [[:break, [:array, [:lit, 1], [:lit, 2]]]],
  "break 1,2,*c" =>
    [[:break, [:argscat, [:array, [:lit, 1], [:lit, 2]], [:vcall,
     :c]]]],

  "break 1,2,3" =>
    [[:break, [:array, [:lit, 1], [:lit, 2], [:lit, 3]]]],

  "break ::A" => [[:break, [:colon3, :A]]],
  "break a rescue b" =>
    [[:rescue, [:break, [:vcall, :a]], [:resbody, nil, [:vcall,
     :b]]]],

  "break a,b,*c do d end" =>
    [[:break, [:argscat, [:array, [:vcall, :a], [:vcall, :b]], [
    :iter, [:fcall, :c], nil, [:vcall, :d]]]]],

  "break b,*c" =>
    [[:break, [:argscat, [:array, [:vcall, :b]], [:vcall, :c]]]],

  "break if /vt100/ =~ line" =>
    [[:if, [:match2, [:lit, /vt100/], [:vcall, :line]], [:break],
     nil]],

  "break ~1" => [[:break, [:call, [:lit, 1], :~]]],
  "break()" => [[:break, [:nil]]],
  "break(1)" => [[:break, [:lit, 1]]],
  "break(a and b)" =>
    [[:break, [:and, [:vcall, :a], [:vcall, :b]]]],

  "break[1]" => [[:break, [:array, [:lit, 1]]]],
  "c a=b do end" =>
    [[:iter, [:fcall, :c, [:array, [:lasgn, :a, [:vcall, :b]]]],
     nil]],

  "c and /a..b/ and c" =>
    [[:and, [:vcall, :c], [:and, [:lit, /a..b/], [:vcall, :c]]]],

  "c and a..b and c" =>
    [[:and, [:vcall, :c], [:and, [:dot2, [:vcall, :a], [:vcall,
     :b]], [:vcall, :c]]]],

  "c do p (110).m end" =>
    [[:iter, [:fcall, :c], nil, [:call, [:fcall, :p, [:array, [
    :lit, 110]]], :m]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "c if !(/a..b/)" =>
    [[:if, [:match, [:lit, /a..b/]], nil, [:vcall, :c]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c if !(a..b)" =>
    [[:if, [:flip2, [:vcall, :a], [:vcall, :b]], nil, [:vcall, :c]
    ]],

  "c if (((((((((/a..b/)))))))))" =>
    [[:if, [:match, [:lit, /a..b/]], [:vcall, :c], nil],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c if (((((((((a..b)))))))))" =>
    [[:if, [:flip2, [:vcall, :a], [:vcall, :b]], [:vcall, :c], nil]
    ],

  "c if ((/a..b/))" =>
    [[:if, [:match, [:lit, /a..b/]], [:vcall, :c], nil],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c if ((a..b))" =>
    [[:if, [:flip2, [:vcall, :a], [:vcall, :b]], [:vcall, :c], nil]
    ],

  "c if (/a..b/)" =>
    [[:if, [:match, [:lit, /a..b/]], [:vcall, :c], nil],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c if (/a..b/) and c" =>
    [[:if, [:and, [:lit, /a..b/], [:vcall, :c]], [:vcall, :c], nil]
    ],

  "c if (a..b)" =>
    [[:if, [:flip2, [:vcall, :a], [:vcall, :b]], [:vcall, :c], nil]
    ],

  "c if (a..b) and c" =>
    [[:if, [:and, [:dot2, [:vcall, :a], [:vcall, :b]], [:vcall,
     :c]], [:vcall, :c], nil]],

  "c if /a..b/" =>
    [[:if, [:match, [:lit, /a..b/]], [:vcall, :c], nil],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c if /a..b/ and c" =>
    [[:if, [:and, [:match, [:lit, /a..b/]], [:vcall, :c]], [
    :vcall, :c], nil],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c if a..b" =>
    [[:if, [:flip2, [:vcall, :a], [:vcall, :b]], [:vcall, :c], nil]
    ],

  "c if a..b and c" =>
    [[:if, [:and, [:flip2, [:vcall, :a], [:vcall, :b]], [:vcall,
     :c]], [:vcall, :c], nil]],

  "c if c and (/a..b/) and c" =>
    [[:if, [:and, [:vcall, :c], [:and, [:lit, /a..b/], [:vcall,
     :c]]], [:vcall, :c], nil]],

  "c if c and (a..b) and c" =>
    [[:if, [:and, [:vcall, :c], [:and, [:dot2, [:vcall, :a], [
    :vcall, :b]], [:vcall, :c]]], [:vcall, :c], nil]],

  "c if c and /a..b/ and c" =>
    [[:if, [:and, [:vcall, :c], [:and, [:match, [:lit, /a..b/]],
     [:vcall, :c]]], [:vcall, :c], nil],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c if c and a..b and c" =>
    [[:if, [:and, [:vcall, :c], [:and, [:flip2, [:vcall, :a], [
    :vcall, :b]], [:vcall, :c]]], [:vcall, :c], nil]],

  "c if not (/a..b/)" =>
    [[:if, [:match, [:lit, /a..b/]], nil, [:vcall, :c]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c if not (a..b)" =>
    [[:if, [:flip2, [:vcall, :a], [:vcall, :b]], nil, [:vcall, :c]
    ]],

  "c if p (1).m" =>
    [[:if, [:call, [:fcall, :p, [:array, [:lit, 1]]], :m], [
    :vcall, :c], nil],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "c or /a..b/ or c" =>
    [[:or, [:vcall, :c], [:or, [:lit, /a..b/], [:vcall, :c]]]],

  "c or a..b or c" =>
    [[:or, [:vcall, :c], [:or, [:dot2, [:vcall, :a], [:vcall, :b]
    ], [:vcall, :c]]]],

  "c unless (/a..b/)" =>
    [[:if, [:match, [:lit, /a..b/]], nil, [:vcall, :c]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c unless (/a..b/) or c" =>
    [[:if, [:or, [:lit, /a..b/], [:vcall, :c]], nil, [:vcall, :c]
    ]],

  "c unless (a..b)" =>
    [[:if, [:flip2, [:vcall, :a], [:vcall, :b]], nil, [:vcall, :c]
    ]],

  "c unless (a..b) or c" =>
    [[:if, [:or, [:dot2, [:vcall, :a], [:vcall, :b]], [:vcall, :c]
    ], nil, [:vcall, :c]]],

  "c unless /a..b/" =>
    [[:if, [:match, [:lit, /a..b/]], nil, [:vcall, :c]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c unless /a..b/ or c" =>
    [[:if, [:or, [:match, [:lit, /a..b/]], [:vcall, :c]], nil, [
    :vcall, :c]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c unless a..b" =>
    [[:if, [:flip2, [:vcall, :a], [:vcall, :b]], nil, [:vcall, :c]
    ]],

  "c unless a..b or c" =>
    [[:if, [:or, [:flip2, [:vcall, :a], [:vcall, :b]], [:vcall,
     :c]], nil, [:vcall, :c]]],

  "c unless c or (/a..b/) or c" =>
    [[:if, [:or, [:vcall, :c], [:or, [:lit, /a..b/], [:vcall, :c]
    ]], nil, [:vcall, :c]]],

  "c unless c or (a..b) or c" =>
    [[:if, [:or, [:vcall, :c], [:or, [:dot2, [:vcall, :a], [
    :vcall, :b]], [:vcall, :c]]], nil, [:vcall, :c]]],

  "c unless c or /a..b/ or c" =>
    [[:if, [:or, [:vcall, :c], [:or, [:match, [:lit, /a..b/]], [
    :vcall, :c]]], nil, [:vcall, :c]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c unless c or a..b or c" =>
    [[:if, [:or, [:vcall, :c], [:or, [:flip2, [:vcall, :a], [
    :vcall, :b]], [:vcall, :c]]], nil, [:vcall, :c]]],

  "c unless p (1).m" =>
    [[:if, [:call, [:fcall, :p, [:array, [:lit, 1]]], :m], nil, [
    :vcall, :c]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "c until p (1).m" =>
    [[:until, [:call, [:fcall, :p, [:array, [:lit, 1]]], :m], [
    :vcall, :c], true],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "c while d and /8/../2.a?(b)/" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:match, [:lit, /8/]
    ], [:match, [:lit, /2.a?(b)/]]]], [:vcall, :c], true],
    {:warnings=>["(string):1: warning: regex literal in condition", "(string):1: warning: regex literal in condition"]}],

  "c while d and /8/..2.a?(b)" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:match, [:lit, /8/]
    ], [:call, [:lit, 2], :a?, [:array, [:vcall, :b]]]]], [
    :vcall, :c], true],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c while d and 2.a?(b)..8" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:call, [:lit, 2], :a?, [
    :array, [:vcall, :b]]], [:call, [:lit, 8], :==, [:array, [
    :gvar, :$.]]]]], [:vcall, :c], true],
    {:warnings=>["(string):1: warning: integer literal in conditional range"]}],

  "c while d and 8../2.a?(b)/" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:call, [:lit, 8], :==, [
    :array, [:gvar, :$.]]], [:match, [:lit, /2.a?(b)/]]]], [
    :vcall, :c], true],
    {:warnings=>["(string):1: warning: integer literal in conditional range", "(string):1: warning: regex literal in condition"]}],

  "c while d and 8..2.a?(b)" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:call, [:lit, 8], :==, [
    :array, [:gvar, :$.]]], [:call, [:lit, 2], :a?, [:array, [
    :vcall, :b]]]]], [:vcall, :c], true],
    {:warnings=>["(string):1: warning: integer literal in conditional range"]}],

  "c while d and 8..:b8" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:call, [:lit, 8], :==, [
    :array, [:gvar, :$.]]], [:lit, :b8]]], [:vcall, :c], true],
    {:warnings=>["(string):1: warning: integer literal in conditional range", "(string):1: warning: literal in condition", "(string):1: warning: range literal in condition"]}],

  "c while d and 8.8..2.a?(b)" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:call, [:lit, 8.8],
     :==, [:array, [:gvar, :$.]]], [:call, [:lit, 2], :a?, [
    :array, [:vcall, :b]]]]], [:vcall, :c], true],
    {:warnings=>["(string):1: warning: literal in condition", "(string):1: warning: range literal in condition"]}],

  "c while d and 888888888888888888888888888888888888888888888888888888..2.a?(b)\
" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:call, [
    :lit, 888888888888888888888888888888888888888888888888888888],
     :==, [:array, [:gvar, :$.]]], [:call, [:lit, 2], :a?, [
    :array, [:vcall, :b]]]]], [:vcall, :c], true],
    {:warnings=>["(string):1: warning: literal in condition", "(string):1: warning: range literal in condition"]}],

  "c while d and :a8..2.a?(b)" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:call, [:lit, :a8],
     :==, [:array, [:gvar, :$.]]], [:call, [:lit, 2], :a?, [
    :array, [:vcall, :b]]]]], [:vcall, :c], true],
    {:warnings=>["(string):1: warning: literal in condition", "(string):1: warning: range literal in condition"]}],

  "c while d and :a8..:b8" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:call, [:lit, :a8],
     :==, [:array, [:gvar, :$.]]], [:lit, :b8]]], [:vcall, :c],
     true],
    {:warnings=>["(string):1: warning: literal in condition", "(string):1: warning: literal in condition", "(string):1: warning: range literal in condition"]}],

  "c while d and a8../2.a?(b)/" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:vcall, :a8], [
    :match, [:lit, /2.a?(b)/]]]], [:vcall, :c], true],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "c while d and a8..:b8" =>
    [[:while, [:and, [:vcall, :d], [:flip2, [:vcall, :a8], [:lit,
     :b8]]], [:vcall, :c], true],
    {:warnings=>["(string):1: warning: literal in condition", "(string):1: warning: range literal in condition"]}],

  "c while p (1).m" =>
    [[:while, [:call, [:fcall, :p, [:array, [:lit, 1]]], :m], [
    :vcall, :c], true],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "c+=begin; a; rescue b; end" =>
    [[:lasgn, :c, [:call, [:lvar, :c], :+, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "c=0,begin; a; rescue b; end" =>
    [[:lasgn, :c, [:svalue, [:array, [:lit, 0], [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "c=0,begin; a; rescue b; end,0" =>
    [[:lasgn, :c, [:svalue, [:array, [:lit, 0], [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]], [:lit, 0]]
    ]]],

  "c=1;a b=c do end" =>
    [[:block, [:lasgn, :c, [:lit, 1]], [:iter, [:fcall, :a, [
    :array, [:lasgn, :b, [:lvar, :c]]]], nil]]],

  "c=begin; a; rescue b; end" =>
    [[:lasgn, :c, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]]]],

  "c=begin; a; rescue b; end,0" =>
    [[:lasgn, :c, [:svalue, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]], [:lit, 0]]]]],

  "ca (:f){}" =>
    [[:iter, [:fcall, :ca, [:array, [:lit, :f]]], nil],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "case $1; when 0,*[2,66]: p 1; when 3; 4 else p 2 end" =>
    [[:case, [:nth_ref, 1], [:when, [:array, [:lit, 0], [:when, [
    :array, [:lit, 2], [:lit, 66]], nil]], [:fcall, :p, [:array,
     [:lit, 1]]]], [:when, [:array, [:lit, 3]], [:lit, 4]], [
    :fcall, :p, [:array, [:lit, 2]]]]],

  "case $1; when 0,*a: p 1; when 3; 4 else p 2 end" =>
    [[:case, [:nth_ref, 1], [:when, [:array, [:lit, 0], [:when, [
    :vcall, :a], nil]], [:fcall, :p, [:array, [:lit, 1]]]], [
    :when, [:array, [:lit, 3]], [:lit, 4]], [:fcall, :p, [:array, [
    :lit, 2]]]]],

  "\
case 1
when *[2]: p 1
else p 2
end\

" =>
    [[:case, [:lit, 1], [:when, [:array, [:when, [:array, [:lit,
     2]], nil]], [:fcall, :p, [:array, [:lit, 1]]]], [:fcall, :p, [
    :array, [:lit, 2]]]]],

  "case F;when G; else;case; when j; end;end" =>
    [[:case, [:const, :F], [:when, [:array, [:const, :G]], nil],
     [:when, [:array, [:vcall, :j]], nil]]],

  "case a; when B,C,D: d end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:const, :B], [
    :const, :C], [:const, :D]], [:vcall, :d]], nil]],

  "case a; when b then c end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b]], [
    :vcall, :c]], nil]],

  "case a; when b,c,d then d end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b], [
    :vcall, :c], [:vcall, :d]], [:vcall, :d]], nil]],

  "case a; when b,c,d: d end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b], [
    :vcall, :c], [:vcall, :d]], [:vcall, :d]], nil]],

  "case a; when b: c end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b]], [
    :vcall, :c]], nil]],

  "case a; when b; c end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b]], [
    :vcall, :c]], nil]],

  "case a; when b; c when d; e end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b]], [
    :vcall, :c]], [:when, [:array, [:vcall, :d]], [:vcall, :e]],
     nil]],

  "case a; when b; c; when d; e else f end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b]], [
    :vcall, :c]], [:when, [:array, [:vcall, :d]], [:vcall, :e]],
     [:vcall, :f]]],

  "case a; when b; c; when d; e end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b]], [
    :vcall, :c]], [:when, [:array, [:vcall, :d]], [:vcall, :e]],
     nil]],

  "case a; when b; when c; end" =>
    [[:case, [:vcall, :a], [:when, [:array, [:vcall, :b]], nil],
     [:when, [:array, [:vcall, :c]], nil], nil]],

  "case a;else;end" => SyntaxError.new("(string):1: syntax error, unexpected kELSE, expecting kWHEN\ncase a;else;end\n           ^"),
  "case g; when *a=r; end" =>
    [[:case, [:vcall, :g], [:when, [:array, [:when, [:lasgn, :a,
     [:vcall, :r]], nil]], nil], nil]],

  "case g; when a=r,b; end" =>
    [[:case, [:vcall, :g], [:when, [:array, [:lasgn, :a, [:vcall,
     :r]], [:vcall, :b]], nil], nil]],

  "case m; when begin; a; rescue b; end,k; j; when n; o end" =>
    [[:case, [:vcall, :m], [:when, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]], [:vcall, :k]], [:vcall,
     :j]], [:when, [:array, [:vcall, :n]], [:vcall, :o]], nil]],

  "case m; when begin; a; rescue b; end; j; when n; o end" =>
    [[:case, [:vcall, :m], [:when, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]], [:vcall, :j]], [:when, [
    :array, [:vcall, :n]], [:vcall, :o]], nil]],

  "case m; when k,begin; a; rescue b; end,k; j; when n; o end" =>
    [[:case, [:vcall, :m], [:when, [:array, [:vcall, :k], [
    :rescue, [:vcall, :a], [:resbody, [:array, [:vcall, :b]]]], [
    :vcall, :k]], [:vcall, :j]], [:when, [:array, [:vcall, :n]],
     [:vcall, :o]], nil]],

  "case m; when k,begin; a; rescue b; end; j; when n; o end" =>
    [[:case, [:vcall, :m], [:when, [:array, [:vcall, :k], [
    :rescue, [:vcall, :a], [:resbody, [:array, [:vcall, :b]]]]],
     [:vcall, :j]], [:when, [:array, [:vcall, :n]], [:vcall, :o]
    ], nil]],

  "case v; when 4; 5; else return ([vUew]).e  end" =>
    [[:case, [:vcall, :v], [:when, [:array, [:lit, 4]], [:lit, 5]
    ], [:return, [:call, [:array, [:vcall, :vUew]], :e]]]],

  "case when true; 4 end" =>
    [[:case, nil, [:when, [:array, [:true]], [:lit, 4]], nil]],

  "case z; when a=b=1,d=2; yy end" =>
    [[:case, [:vcall, :z], [:when, [:array, [:lasgn, :a, [:lasgn,
     :b, [:lit, 1]]], [:lasgn, :d, [:lit, 2]]], [:vcall, :yy]],
     nil]],

  "case; else; end" => [],
  "case; when b; c end" =>
    [[:case, nil, [:when, [:array, [:vcall, :b]], [:vcall, :c]],
     nil]],

  "case; when false; else case; when nil; else 5; end; end" =>
    [[:case, nil, [:when, [:array, [:false]], nil], [:when, [
    :array, [:nil]], nil]]],

  "case;else case; else; end;end" => [],
  "case;when 0;ng = 'JIS';else case; when sjis__length; ding =\
 'EUC-JP' ;end;end" =>
    [[:case, nil, [:when, [:array, [:lit, 0]], [:lasgn, :ng, [
    :str, "JIS"]]], [:when, [:array, [:vcall, :sjis__length]], [
    :lasgn, :ding, [:str, "EUC-JP"]]]]],

  "case;when I; JIS;else case; when sjis__length; EJP ;else 55;\
 end;end" =>
    [[:case, nil, [:when, [:array, [:const, :I]], [:const, :JIS]
    ], [:when, [:array, [:vcall, :sjis__length]], [:const, :EJP]
    ]]],

  "\
case
 when true; 4 end" =>
    [[:case, nil, [:when, [:array, [:true]], [:lit, 4]], nil]],

  "catch * @com_disk" =>
    [[:call, [:vcall, :catch], :*, [:array, [:ivar, :@com_disk]]
    ]],

  "catch - @com_disk" =>
    [[:call, [:vcall, :catch], :-, [:array, [:ivar, :@com_disk]]
    ]],

  "class A::B; end" =>
    [[:class, [:colon2, [:const, :A], :B], nil, [:scope]]],

  "class A; b end" => [[:class, :A, nil, [:scope, [:vcall, :b]]]],
  "class A; b; rescue C=>d; e; else g; ensure f; end" =>
    [[:class, :A, nil, [:scope, [:ensure, [:rescue, [:vcall, :b], [
    :resbody, [:array, [:const, :C]], [:block, [:lasgn, :d, [
    :gvar, :$!]], [:vcall, :e]]], [:vcall, :g]], [:vcall, :f]]]]
    ],

  "class A; b;c end" =>
    [[:class, :A, nil, [:scope, [:block, [:vcall, :b], [:vcall,
     :c]]]]],

  "class A; end" => [[:class, :A, nil, [:scope]]],
  "class A;def b;class <<self;\"\#{@@p = false}\" end;end;end" =>
    [[:class, :A, nil, [:scope, [:defn, :b, [:scope, [:block, [
    :args], [:sclass, [:self], [:scope, [:dstr, "", [:evstr, [
    :cvdecl, :@@p, [:false]]]]]]]]]]]],

  "class A;def b;class <<self;@@p = false end;end;end" =>
    [[:class, :A, nil, [:scope, [:defn, :b, [:scope, [:block, [
    :args], [:sclass, [:self], [:scope, [:cvdecl, :@@p, [:false]
    ]]]]]]]]],

  "class A<B; c end" =>
    [[:class, :A, [:const, :B], [:scope, [:vcall, :c]]]],

  "class A<B; c;d end" =>
    [[:class, :A, [:const, :B], [:scope, [:block, [:vcall, :c], [
    :vcall, :d]]]]],

  "class A<B; end" => [[:class, :A, [:const, :B], [:scope]]],
  "class BUG_ONE extend OptiFlag::Flagset; end" => SyntaxError.new("(string):1: syntax error, unexpected tIDENTIFIER, expecting '<' or '\\n' or ';'\nclass BUG_ONE extend OptiFlag::Flagset; end\n                    ^"),
  "class BUNE end" => SyntaxError.new("(string):1: syntax error, unexpected kEND, expecting '<' or '\\n' or ';'\nclass BUNE end\n              ^"),
  "class BUNE; extend end" =>
    [[:class, :BUNE, nil, [:scope, [:vcall, :extend]]]],

  "\
class F0000; end
def F0000; end\

" =>
    [[:block, [:class, :F0000, nil, [:scope]], [:defn, :F0000, [
    :scope, [:block, [:args], [:nil]]]]]],

  "\
class Fixnum
  public :`
  def `(s)
    print \"bq: \#{s}\\n\"
  end
end
69.`('what a world')
79::`('what a word')\

" =>
    [[:block, [:class, :Fixnum, nil, [:scope, [:block, [:fcall,
     :public, [:array, [:lit, :`]]], [:defn, :`, [:scope, [:block, [
    :args, :s], [:fcall, :print, [:array, [:dstr, "bq: ", [:evstr, [
    :lvar, :s]], [:str, "\n"]]]]]]]]]], [:call, [:lit, 69], :`, [
    :array, [:str, "what a world"]]], [:call, [:lit, 79], :`, [
    :array, [:str, "what a word"]]]]],

  "\
class Foop
  def Foop.bar a,b
    p a,b
  end
end
Foop.bar 1,2
Foop::bar 3,4\

" =>
    [[:block, [:class, :Foop, nil, [:scope, [:defs, [
    :const, :Foop], :bar, [:scope, [:block, [:args, :a, :b], [
    :fcall, :p, [:array, [:lvar, :a], [:lvar, :b]]]]]]]], [:call, [
    :const, :Foop], :bar, [:array, [:lit, 1], [:lit, 2]]], [
    :call, [:const, :Foop], :bar, [:array, [:lit, 3], [:lit, 4]]
    ]]],

  "\
class Foou
 public
 def [] x=-100,&y; p x; 100 end
end\

" =>
    [[:class, :Foou, nil, [:scope, [:block, [:vcall, :public], [
    :defn, :[], [:scope, [:block, [:args, :x, [:block, [:lasgn,
     :x, [:lit, -100]]]], [:block_arg, :y], [:fcall, :p, [:array, [
    :lvar, :x]]], [:lit, 100]]]]]]]],

  "\
class Hosts
end
class DNS < Hosts
end\

" =>
    [[:block, [:class, :Hosts, nil, [:scope]], [:class, :DNS, [
    :const, :Hosts], [:scope]]]],

  "class NilClass; undef +,[] end" =>
    [[:class, :NilClass, nil, [:scope, [:block, [:undef, [:lit,
     :+]], [:undef, [:lit, :[]]]]]]],

  "\
class String
class Class
end
end\

" =>
    [[:class, :String, nil, [:scope, [:class, :Class, nil, [
    :scope]]]]],

  "class Using_Paths < Doc < FigureMaker; end" =>
    [[:class, :Using_Paths, [:call, [:const, :Doc], :<, [:array,
     [:const, :FigureMaker]]], [:scope]]],

  "class __FILE__::A; b end" =>
    [[:class, [:colon2, [:str, "(string)"], :A], nil, [:scope, [
    :vcall, :b]]]],

  "class __LINE__::A; b end" =>
    [[:class, [:colon2, [:lit, 1], :A], nil, [:scope, [:vcall, :b]
    ]]],

  "class a.b.c.d.e.f::Quux; YYY=534 end" =>
    [[:class, [:colon2, [:call, [:call, [:call, [:call, [:call, [
    :vcall, :a], :b], :c], :d], :e], :f], :Quux], nil, [:scope, [
    :cdecl, :YYY, [:lit, 534]]]]],

  "class self::A; b end" =>
    [[:class, [:colon2, [:self], :A], nil, [:scope, [:vcall, :b]
    ]]],

  "class self::A<B; c end" =>
    [[:class, [:colon2, [:self], :A], [:const, :B], [:scope, [
    :vcall, :c]]]],

  "class subject::Builder; foo end" =>
    [[:class, [:colon2, [:vcall, :subject], :Builder], nil, [
    :scope, [:vcall, :foo]]]],

  "class subject::Builder<T; foo end" =>
    [[:class, [:colon2, [:vcall, :subject], :Builder], [:const,
     :T], [:scope, [:vcall, :foo]]]],

  "class<<A; b; rescue C=>d; e; else g; ensure f; end" =>
    [[:sclass, [:const, :A], [:scope, [:ensure, [:rescue, [
    :vcall, :b], [:resbody, [:array, [:const, :C]], [:block, [
    :lasgn, :d, [:gvar, :$!]], [:vcall, :e]]], [:vcall, :g]], [
    :vcall, :f]]]]],

  "class<<a; b end" =>
    [[:sclass, [:vcall, :a], [:scope, [:vcall, :b]]]],

  "class<<a; b;c end" =>
    [[:sclass, [:vcall, :a], [:scope, [:block, [:vcall, :b], [
    :vcall, :c]]]]],

  "class<<a; begin; r; t end;c end" =>
    [[:sclass, [:vcall, :a], [:scope, [:block, [:vcall, :r], [
    :vcall, :t], [:vcall, :c]]]]],

  "class<<a; end" => [[:sclass, [:vcall, :a], [:scope]]],
  "class[Array][0]::Foo; Bazz=556 end" =>
    [[:class, [:colon2, [:call, [:array, [:const, :Array]], :[],
     [:array, [:lit, 0]]], :Foo], nil, [:scope, [:cdecl, :Bazz, [
    :lit, 556]]]]],

  "\
class
A; end" =>
    [[:class, :A, nil, [:scope]]],

  "\
class
A<B; end" =>
    [[:class, :A, [:const, :B], [:scope]]],

  "\
contents << <<HEADER\r
class \#{filename.capitalize}TestCase < Math3d::TestCase\r
\r
        def set_up\r
                @\#{filename.downcase}s = []\r
                @\#{filename.downcase}s.clear\r
        end\r
\r
HEADER\r
\"\" << <<Stupid\r
end\r
Stupid\r
\r\

" =>
    [[:block, [:call, [:vcall, :contents], :<<, [:array, [:dstr,
     "class ", [:evstr, [:call, [:vcall, :filename], :capitalize]
    ], [:str, "TestCase < Math3d::TestCase\n\n        def set_up\n                @"], [
    :evstr, [:call, [:vcall, :filename], :downcase]], [
    :str, "s = []\n                @"], [:evstr, [:call, [:vcall,
     :filename], :downcase]], [:str, "s.clear\n        end\n\n"]
    ]]], [:call, [:str, ""], :<<, [:array, [:str, "end\n"]]]]],

  "continue * @com_disk" =>
    [[:call, [:vcall, :continue], :*, [:array, [:ivar, :@com_disk]
    ]]],

  "continue - @com_disk" =>
    [[:call, [:vcall, :continue], :-, [:array, [:ivar, :@com_disk]
    ]]],

  "continue[1]" =>
    [[:call, [:vcall, :continue], :[], [:array, [:lit, 1]]]],

  "d=a=b do end" =>
    [[:lasgn, :d, [:lasgn, :a, [:iter, [:fcall, :b], nil]]]],

  "d=e,*f" =>
    [[:lasgn, :d, [:svalue, [:argscat, [:array, [:vcall, :e]], [
    :vcall, :f]]]]],

  "d=e,f" =>
    [[:lasgn, :d, [:svalue, [:array, [:vcall, :e], [:vcall, :f]]
    ]]],

  "ddd=\"ddd\"" => [[:lasgn, :ddd, [:str, "ddd"]]],
  "\
def (  def test_endblockwarn
    ruby = EnvUtil.rubybin
    # Use Tempfile to create temporary file path.
    launcher = Tempfile.new(self.class.name)
    errout = Tempfile.new(self.class.name)
    launcher << <<EOF
errout = ARGV.shift
STDERR.reopen(File.open(errout, \"w\"))
STDERR.sync = true
Dir.chdir(\#{q(DIR)})
cmd = \"\\\"\#{ruby}\\\" \\\"endblockwarn.rb\\\"\"
system(cmd)
EOF
    launcher.close
    launcherpath = launcher.path
    errout.close
    erroutpath = errout.path
    system(\"\#{q(ruby)} \#{q(launcherpath)} \#{q(erroutpath)}\")
    expected = <<EOW
endblockwarn.rb:2: warning: END in method; use at_exit
(eval):2: warning: END in method; use at_exit
EOW
    assert_equal(expected, File.read(erroutpath))
    # expecting Tempfile to unlink launcher and errout file.
  end).foo; end" =>
    [[:defs, [:defn, :test_endblockwarn, [:scope, [:block, [
    :args], [:lasgn, :ruby, [:call, [:const, :EnvUtil], :rubybin]
    ], [:lasgn, :launcher, [:call, [:const, :Tempfile], :new, [
    :array, [:call, [:call, [:self], :class], :name]]]], [:lasgn,
     :errout, [:call, [:const, :Tempfile], :new, [:array, [:call, [
    :call, [:self], :class], :name]]]], [:call, [:lvar, :launcher],
     :<<, [:array, [
    :dstr, "errout = ARGV.shift\nSTDERR.reopen(File.open(errout, \"w\"))\nSTDERR.sync = true\nDir.chdir(", [
    :evstr, [:fcall, :q, [:array, [:const, :DIR]]]], [
    :str, ")\ncmd = \"\""], [:evstr, [:lvar, :ruby]], [
    :str, "\" \"endblockwarn.rb\"\"\nsystem(cmd)\n"]]]], [:call,
     [:lvar, :launcher], :close], [:lasgn, :launcherpath, [:call, [
    :lvar, :launcher], :path]], [:call, [:lvar, :errout], :close], [
    :lasgn, :erroutpath, [:call, [:lvar, :errout], :path]], [
    :fcall, :system, [:array, [:dstr, "", [:evstr, [:fcall, :q, [
    :array, [:lvar, :ruby]]]], [:str, " "], [:evstr, [:fcall, :q, [
    :array, [:lvar, :launcherpath]]]], [:str, " "], [:evstr, [
    :fcall, :q, [:array, [:lvar, :erroutpath]]]]]]], [:lasgn,
     :expected, [
    :str, "endblockwarn.rb:2: warning: END in method; use at_exit\n(eval):2: warning: END in method; use at_exit\n"]], [
    :fcall, :assert_equal, [:array, [:lvar, :expected], [:call, [
    :const, :File], :read, [:array, [:lvar, :erroutpath]]]]]]]],
     :foo, [:scope, [:args]]],
    {:warnings=>["(string):1: warning: void value expression"]}],

  "\
def (  def test_endblockwarn
<<EOF
irqDIR
cmd
EOF
<<EOW
endblat_exit
EOW
  end).foo; end" =>
    [[:defs, [:defn, :test_endblockwarn, [:scope, [:block, [
    :args], [:str, "endblat_exit\n"]]]], :foo, [:scope, [:args]]
    ],
    {:warnings=>["(string):2: warning: unused literal ignored", "(string):1: warning: void value expression"]}],

  "def ((begin; a; rescue b; end)).m; end" =>
    [[:defs, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], :m, [:scope, [:args]]]],

  "def (@@foo=bar).baz; quux end" =>
    [[:defs, [:cvdecl, :@@foo, [:vcall, :bar]], :baz, [:scope, [
    :block, [:args], [:vcall, :quux]]]]],

  "\
def (
<<EOF
irqDIR
cmd
EOF
<<EOW
endblat_exit
EOW
).foo; end" =>
    SyntaxError.new("(string):6: syntax error, unexpected tSTRING_BEG, expecting ')'\n(string):9: syntax error, unexpected ')', expecting $end\n).foo; end\n ^"),

  "\
def (
__END__
).foo; end" =>
    SyntaxError.new("(string):2: syntax error, unexpected $end\n__END__\n ^"),

  "def (a).b; end" =>
    [[:defs, [:vcall, :a], :b, [:scope, [:args]]]],

  "def (a.b.c).b; end" =>
    [[:defs, [:call, [:call, [:vcall, :a], :b], :c], :b, [:scope, [
    :args]]]],

  "\
def (a=
__END__
).foo; end" =>
    SyntaxError.new("(string):2: syntax error, unexpected $end\n__END__\n ^"),

  "def (begin; a; rescue b; end).m; end" =>
    [[:defs, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], :m, [:scope, [:args]]]],

  "def (is_a?(File::Stat)).foofarendle;end" =>
    [[:defs, [:fcall, :is_a?, [:array, [:colon2, [:const, :File],
     :Stat]]], :foofarendle, [:scope, [:args]]]],

  "def ==(o) 444 end" =>
    [[:defn, :==, [:scope, [:block, [:args, :o], [:lit, 444]]]]],

  "def A.b; end" => [[:defs, [:const, :A], :b, [:scope, [:args]]]],
  "def Class.-@; [:-@, self] end" =>
    [[:defs, [:const, :Class], :-@, [:scope, [:block, [:args], [
    :array, [:lit, :-@], [:self]]]]]],

  "def String.*(right) [self,right] end" =>
    [[:defs, [:const, :String], :*, [:scope, [:block, [:args, :right], [
    :array, [:self], [:lvar, :right]]]]]],

  "def String./(right) [self,:/,right] end" =>
    [[:defs, [:const, :String], :/, [:scope, [:block, [:args, :right], [
    :array, [:self], [:lit, :/], [:lvar, :right]]]]]],

  "def String.<<(right) [self,:<<,right] end" =>
    [[:defs, [:const, :String], :<<, [:scope, [:block, [:args,
     :right], [:array, [:self], [:lit, :<<], [:lvar, :right]]]]]
    ],

  "def String.[](right) [self,:[],right] end" =>
    [[:defs, [:const, :String], :[], [:scope, [:block, [:args,
     :right], [:array, [:self], [:lit, :[]], [:lvar, :right]]]]]
    ],

  "def String.ffff4() self.to_s+\"ffff\" end" =>
    [[:defs, [:const, :String], :ffff4, [:scope, [:block, [:args], [
    :call, [:call, [:self], :to_s], :+, [:array, [:str, "ffff"]]
    ]]]]],

  "\
def 
A; b end" =>
    [[:defn, :A, [:scope, [:block, [:args], [:vcall, :b]]]]],

  "def __DATA__.f; end" =>
    [[:defs, [:vcall, :__DATA__], :f, [:scope, [:args]]]],

  "def __END__.x; end" =>
    [[:defs, [:vcall, :__END__], :x, [:scope, [:args]]]],

  "def __END__; end" =>
    [[:defn, :__END__, [:scope, [:block, [:args], [:nil]]]]],

  "def __FILE__.x; end" =>
    [[:defs, [:str, "(string)"], :x, [:scope, [:args]]]],

  "def __FILE__; end" =>
    [[:defn, :__FILE__, [:scope, [:block, [:args], [:nil]]]]],

  "def __LINE__.x; end" =>
    [[:defs, [:lit, 1], :x, [:scope, [:args]]]],

  "def __LINE__; end" =>
    [[:defn, :__LINE__, [:scope, [:block, [:args], [:nil]]]]],

  "def a b,d=e; end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :d, [:block, [
    :lasgn, :d, [:vcall, :e]]]], [:nil]]]]],

  "def a b,d=p (1.10).m,*f,&g; end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :d, :"*f", [:block, [
    :lasgn, :d, [:call, [:fcall, :p, [:array, [:lit, 1.1]]], :m]
    ]]], [:block_arg, :g], [:nil]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "def a b=@@foo=bar; end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, [:block, [:lasgn,
     :b, [:cvasgn, :@@foo, [:vcall, :bar]]]]], [:nil]]]]],

  "def a b=c,d=e,&f; end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :d, [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:vcall, :e]]]], [
    :block_arg, :f], [:nil]]]]],

  "def a b=c,d=e,&f; g end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :d, [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:vcall, :e]]]], [
    :block_arg, :f], [:vcall, :g]]]]],

  "def a b=c,d=e,*f,&g; end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :d, :"*f", [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:vcall, :e]]]], [
    :block_arg, :g], [:nil]]]]],

  "def a b=c,d=e,*f,&g; h end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :d, :"*f", [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:vcall, :e]]]], [
    :block_arg, :g], [:vcall, :h]]]]],

  "def a b=c,d=e,*f; end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :d, :"*f", [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:vcall, :e]]]], [
    :nil]]]]],

  "def a b=c,d=e,*f; g end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :d, :"*f", [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:vcall, :e]]]], [
    :vcall, :g]]]]],

  "def a b=c,d=e; end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :d, [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:vcall, :e]]]], [
    :nil]]]]],

  "def a b=c,d=e; f end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :d, [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:vcall, :e]]]], [
    :vcall, :f]]]]],

  "def a b=c; end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, [:block, [:lasgn,
     :b, [:vcall, :c]]]], [:nil]]]]],

  "def a c=d=1; end" =>
    [[:defn, :a, [:scope, [:block, [:args, :d, [:block, [:lasgn,
     :c, [:lasgn, :d, [:lit, 1]]]]], [:nil]]]]],

  "def a() end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:nil]]]]],

  "def a(b) c end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b], [:vcall, :c]]]]],

  "def a(b,c) d,e=f,g end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :c], [:masgn, [
    :array, [:lasgn, :d], [:lasgn, :e]], nil, [:array, [:vcall,
     :f], [:vcall, :g]]]]]]],

  "def a(b,c) e=*g end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :c], [:lasgn, :e,
     [:svalue, [:splat, [:vcall, :g]]]]]]]],

  "def a(b,c,d,e=f,g=h,*i,&j) k;l;m rescue n; rescue o,p,q=>r:\
 s; rescue t; u; rescue; v else w ensure x end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, :c, :d, :e, :g,
     :"*i", [:block, [:lasgn, :e, [:vcall, :f]], [:lasgn, :g, [
    :vcall, :h]]]], [:block_arg, :j], [:ensure, [:rescue, [:block, [
    :vcall, :k], [:vcall, :l], [:rescue, [:vcall, :m], [:resbody,
     nil, [:vcall, :n]]]], [:resbody, [:array, [:vcall, :o], [
    :vcall, :p], [:vcall, :q]], [:block, [:lasgn, :r, [:gvar, :$!]
    ], [:vcall, :s]], [:resbody, [:array, [:vcall, :t]], [:vcall,
     :u], [:resbody, nil, [:vcall, :v]]]], [:vcall, :w]], [
    :vcall, :x]]]]]],

  "def a(b=(@@foo=bar)) end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, [:block, [:lasgn,
     :b, [:cvasgn, :@@foo, [:vcall, :bar]]]]], [:nil]]]]],

  "def a(b=@@foo=bar) end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, [:block, [:lasgn,
     :b, [:cvasgn, :@@foo, [:vcall, :bar]]]]], [:nil]]]]],

  "def a(b=nil) c end" =>
    [[:defn, :a, [:scope, [:block, [:args, :b, [:block, [:lasgn,
     :b, [:nil]]]], [:vcall, :c]]]]],

  "def a.b i=1; c end" =>
    [[:defs, [:vcall, :a], :b, [:scope, [:block, [:args, :i, [
    :block, [:lasgn, :i, [:lit, 1]]]], [:vcall, :c]]]]],

  "\
def a.b(c=
__END__
); end" =>
    SyntaxError.new("(string):2: syntax error, unexpected $end\n__END__\n ^"),

  "def a.b; @@c=d; end" =>
    [[:defs, [:vcall, :a], :b, [:scope, [:block, [:args], [
    :cvasgn, :@@c, [:vcall, :d]]]]]],

  "def a.b; c end" =>
    [[:defs, [:vcall, :a], :b, [:scope, [:block, [:args], [
    :vcall, :c]]]]],

  "def a.b; end" => [[:defs, [:vcall, :a], :b, [:scope, [:args]]]],
  "def a.b; end rescue b0" =>
    [[:rescue, [:defs, [:vcall, :a], :b, [:scope, [:args]]], [
    :resbody, nil, [:vcall, :b0]]]],

  "def a.b= i=1; c end" =>
    [[:defs, [:vcall, :a], :b=, [:scope, [:block, [:args, :i, [
    :block, [:lasgn, :i, [:lit, 1]]]], [:vcall, :c]]]]],

  "def a.b=(i=1); c end" =>
    [[:defs, [:vcall, :a], :b=, [:scope, [:block, [:args, :i, [
    :block, [:lasgn, :i, [:lit, 1]]]], [:vcall, :c]]]]],

  "def a.b=; c end" =>
    [[:defs, [:vcall, :a], :b=, [:scope, [:block, [:args], [
    :vcall, :c]]]]],

  "def a.b=i=1; c end" =>
    [[:defs, [:vcall, :a], :b=, [:scope, [:block, [:args, :i, [
    :block, [:lasgn, :i, [:lit, 1]]]], [:vcall, :c]]]]],

  "def a.~@; end" =>
    [[:defs, [:vcall, :a], :~, [:scope, [:args]]]],

  "def a0.a  b=c do end; hhh end" =>
    [[:defs, [:vcall, :a0], :a, [:scope, [:block, [:args, :b, [
    :block, [:lasgn, :b, [:iter, [:fcall, :c], nil]]]], [:vcall,
     :hhh]]]]],

  "def a0.a & b; end" =>
    [[:defs, [:vcall, :a0], :a, [:scope, [:block, [:args], [
    :block_arg, :b]]]]],

  "def a0.a * b; end" =>
    [[:defs, [:vcall, :a0], :a, [:scope, [:args, :"*b"]]]],

  "def a0.a b=c,d=e do f end; end" =>
    [[:defs, [:vcall, :a0], :a, [:scope, [:args, :b, :d, [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:iter, [:fcall, :e],
     nil, [:vcall, :f]]]]]]]],

  "def a0.a b=c,d=e e2 do f end; end" => SyntaxError.new("(string):1: syntax error, unexpected tIDENTIFIER, expecting kDO or '{' or '('\ndef a0.a b=c,d=e e2 do f end; end\n                   ^\n(string):1: syntax error, unexpected kEND, expecting $end\ndef a0.a b=c,d=e e2 do f end; end\n                                 ^"),
  "def a0.a b=c,d=e e2 e3 do f end; end" => SyntaxError.new("(string):1: syntax error, unexpected tIDENTIFIER, expecting kDO or '{' or '('\ndef a0.a b=c,d=e e2 e3 do f end; end\n                   ^\n(string):1: syntax error, unexpected kEND, expecting $end\ndef a0.a b=c,d=e e2 e3 do f end; end\n                                    ^"),
  "def a0.a b=c,d=e e2 e3 e4 do f end; end" => SyntaxError.new("(string):1: syntax error, unexpected tIDENTIFIER, expecting kDO or '{' or '('\ndef a0.a b=c,d=e e2 e3 e4 do f end; end\n                   ^\n(string):1: syntax error, unexpected kEND, expecting $end\ndef a0.a b=c,d=e e2 e3 e4 do f end; end\n                                       ^"),
  "def a0.a b=c,d=e() do f end; end" =>
    [[:defs, [:vcall, :a0], :a, [:scope, [:args, :b, :d, [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:iter, [:fcall, :e],
     nil, [:vcall, :f]]]]]]]],

  "def a0.a& b; end" =>
    [[:defs, [:vcall, :a0], :a, [:scope, [:block, [:args], [
    :block_arg, :b]]]]],

  "def a0.a&b; end" =>
    [[:defs, [:vcall, :a0], :a, [:scope, [:block, [:args], [
    :block_arg, :b]]]]],

  "def a0.a( b=c,d=e do f end ); end" =>
    [[:defs, [:vcall, :a0], :a, [:scope, [:args, :b, :d, [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:iter, [:fcall, :e],
     nil, [:vcall, :f]]]]]]]],

  "def a0.a( b=c,d=e() do f end ); end" =>
    [[:defs, [:vcall, :a0], :a, [:scope, [:args, :b, :d, [:block, [
    :lasgn, :b, [:vcall, :c]], [:lasgn, :d, [:iter, [:fcall, :e],
     nil, [:vcall, :f]]]]]]]],

  "def a0.a* b; end" =>
    [[:defs, [:vcall, :a0], :a, [:scope, [:args, :"*b"]]]],

  "def a0.a*b; end" =>
    [[:defs, [:vcall, :a0], :a, [:scope, [:args, :"*b"]]]],

  "def a0unde;  undef new, ne; end" =>
    [[:defn, :a0unde, [:scope, [:block, [:args], [:block, [:undef, [
    :lit, :new]], [:undef, [:lit, :ne]]]]]]],

  "\
def a1(a2=  %s[\\
]);end" =>
    [[:defn, :a1, [:scope, [:block, [:args, :a2, [:block, [
    :lasgn, :a2, [:lit, :"\\\n"]]]], [:nil]]]]],

  "def a::~@; end" =>
    [[:defs, [:vcall, :a], :~, [:scope, [:args]]]],

  "def a; 0..0 end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:lit, 0..0]]]]],

  "def a; @@b=c; end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:cvasgn, :@@b, [
    :vcall, :c]]]]]],

  "def a; d;e;else b; ensure c end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:ensure, [:block, [
    :vcall, :d], [:vcall, :e], [:vcall, :b]], [:vcall, :c]]]]],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "def a; d;e;rescue; b; else c end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:rescue, [:block, [
    :vcall, :d], [:vcall, :e]], [:resbody, nil, [:vcall, :b]], [
    :vcall, :c]]]]]],

  "def a; d;e;rescue; b; ensure c end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:ensure, [:rescue, [
    :block, [:vcall, :d], [:vcall, :e]], [:resbody, nil, [:vcall,
     :b]]], [:vcall, :c]]]]]],

  "def a; e;f; rescue; b; else c; ensure d end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:ensure, [:rescue, [
    :block, [:vcall, :e], [:vcall, :f]], [:resbody, nil, [:vcall,
     :b]], [:vcall, :c]], [:vcall, :d]]]]]],

  "def a; else b end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:vcall, :b]]]],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "def a; else b; ensure c end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:ensure, [:vcall, :b], [
    :vcall, :c]]]]],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "def a; ensure b end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:ensure, [:vcall, :b]
    ]]]]],

  "def a; l=if b; else; end; super *l; end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:lasgn, :l, [:if, [
    :vcall, :b], nil, nil]], [:super, [:splat, [:lvar, :l]]]]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "def a; rescue; b; else c end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:rescue, [:resbody,
     nil, [:vcall, :b]], [:vcall, :c]]]]]],

  "def a; rescue; b; else c; ensure d end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:ensure, [:rescue, [
    :resbody, nil, [:vcall, :b]], [:vcall, :c]], [:vcall, :d]]]]
    ]],

  "def a; rescue; b; end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:rescue, [:resbody,
     nil, [:vcall, :b]]]]]]],

  "def a; rescue; b; ensure c end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:ensure, [:rescue, [
    :resbody, nil, [:vcall, :b]]], [:vcall, :c]]]]]],

  "def a; super *c; end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:super, [:splat, [
    :vcall, :c]]]]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "def a; super b,*c; end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:super, [:argscat, [
    :array, [:vcall, :b]], [:vcall, :c]]]]]]],

  "def a;d;e; else b end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:vcall, :d], [
    :vcall, :e], [:vcall, :b]]]],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "def a;d;e; ensure b end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:ensure, [:block, [
    :vcall, :d], [:vcall, :e]], [:vcall, :b]]]]]],

  "def a;d;e; rescue; b; end" =>
    [[:defn, :a, [:scope, [:block, [:args], [:rescue, [:block, [
    :vcall, :d], [:vcall, :e]], [:resbody, nil, [:vcall, :b]]]]]
    ]],

  "def alias; end" =>
    [[:defn, :alias, [:scope, [:block, [:args], [:nil]]]]],

  "def b i=1; c end" =>
    [[:defn, :b, [:scope, [:block, [:args, :i, [:block, [:lasgn,
     :i, [:lit, 1]]]], [:vcall, :c]]]]],

  "def b; c end" =>
    [[:defn, :b, [:scope, [:block, [:args], [:vcall, :c]]]]],

  "def bar a=b=1,d=2;  end" =>
    [[:defn, :bar, [:scope, [:block, [:args, :b, :a, [:block, [
    :lasgn, :a, [:lasgn, :b, [:lit, 1]]], [:lasgn, :d, [:lit, 2]
    ]]], [:nil]]]]],

  "def bar(a=b=1,d=2)  end" =>
    [[:defn, :bar, [:scope, [:block, [:args, :b, :a, [:block, [
    :lasgn, :a, [:lasgn, :b, [:lit, 1]]], [:lasgn, :d, [:lit, 2]
    ]]], [:nil]]]]],

  "def begin; end" =>
    [[:defn, :begin, [:scope, [:block, [:args], [:nil]]]]],

  "def break.x; end" => SyntaxError.new("(string):1: syntax error, unexpected '.', expecting '\\n' or ';'\ndef break.x; end\n          ^\n(string):1: syntax error, unexpected kEND, expecting $end\ndef break.x; end\n                ^"),
  "def break; end" =>
    [[:defn, :break, [:scope, [:block, [:args], [:nil]]]]],

  "def catch.x; end" =>
    [[:defs, [:vcall, :catch], :x, [:scope, [:args]]]],

  "def catch; end" =>
    [[:defn, :catch, [:scope, [:block, [:args], [:nil]]]]],

  "def class; end" =>
    [[:defn, :class, [:scope, [:block, [:args], [:nil]]]]],

  "def continue.f; end" =>
    [[:defs, [:vcall, :continue], :f, [:scope, [:args]]]],

  "def continue.x; end" =>
    [[:defs, [:vcall, :continue], :x, [:scope, [:args]]]],

  "def continue; end" =>
    [[:defn, :continue, [:scope, [:block, [:args], [:nil]]]]],

  "def d()end" =>
    [[:defn, :d, [:scope, [:block, [:args], [:nil]]]]],

  "def d(dd)end" =>
    [[:defn, :d, [:scope, [:block, [:args, :dd], [:nil]]]]],

  "def d; return (block_given? ? begin; yield f; ensure; f.close;\
 end : f); end" =>
    [[:defn, :d, [:scope, [:block, [:args], [:return, [:if, [
    :fcall, :block_given?], [:ensure, [:yield, [:vcall, :f]], [
    :call, [:vcall, :f], :close]], [:vcall, :f]]]]]]],

  "def d;e install_dir;end" =>
    [[:defn, :d, [:scope, [:block, [:args], [:fcall, :e, [:array, [
    :vcall, :install_dir]]]]]]],

  "def d;end" =>
    [[:defn, :d, [:scope, [:block, [:args], [:nil]]]]],

  "def def; end" =>
    [[:defn, :def, [:scope, [:block, [:args], [:nil]]]]],

  "def defined?(foo) () end" =>
    [[:defn, :defined?, [:scope, [:block, [:args, :foo], [:nil]]
    ]]],

  "def defined?(foo) [] end" =>
    [[:defn, :defined?, [:scope, [:block, [:args, :foo], [:zarray]
    ]]]],

  "def defined?(foo) end" =>
    [[:defn, :defined?, [:scope, [:block, [:args, :foo], [:nil]]
    ]]],

  "def defined?(foo) hh end" =>
    [[:defn, :defined?, [:scope, [:block, [:args, :foo], [:vcall,
     :hh]]]]],

  "def defined?(foo) nil end" =>
    [[:defn, :defined?, [:scope, [:block, [:args, :foo], [:nil]]
    ]]],

  "def defined?(foo) {} end" =>
    [[:defn, :defined?, [:scope, [:block, [:args, :foo], [:hash]
    ]]]],

  "def defined?.x; end" => SyntaxError.new("(string):1: syntax error, unexpected '.', expecting '\\n' or ';'\ndef defined?.x; end\n             ^\n(string):1: syntax error, unexpected kEND, expecting $end\ndef defined?.x; end\n                   ^"),
  "def defined?; end" =>
    [[:defn, :defined?, [:scope, [:block, [:args], [:nil]]]]],

  "def do; end" =>
    [[:defn, :do, [:scope, [:block, [:args], [:nil]]]]],

  "def e; begin; y; g else t; m; end ;end" =>
    [[:defn, :e, [:scope, [:block, [:args], [:block, [:vcall, :y], [
    :vcall, :g], [:vcall, :t], [:vcall, :m]]]]],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "def else; end" =>
    [[:defn, :else, [:scope, [:block, [:args], [:nil]]]]],

  "def elsif; end" =>
    [[:defn, :elsif, [:scope, [:block, [:args], [:nil]]]]],

  "def empty() end" =>
    [[:defn, :empty, [:scope, [:block, [:args], [:nil]]]]],

  "def end; end" =>
    [[:defn, :end, [:scope, [:block, [:args], [:nil]]]]],

  "def ensure; end" =>
    [[:defn, :ensure, [:scope, [:block, [:args], [:nil]]]]],

  "def environment(env = File.basename($0, '.*')) end" =>
    [[:defn, :environment, [:scope, [:block, [:args, :env, [
    :block, [:lasgn, :env, [:call, [:const, :File], :basename, [
    :array, [:gvar, :$0], [:str, ".*"]]]]]], [:nil]]]]],

  "def evaluate rule;  rescue Exception;    puts \"\";  end" =>
    [[:defn, :evaluate, [:scope, [:block, [:args, :rule], [
    :rescue, [:resbody, [:array, [:const, :Exception]], [:fcall,
     :puts, [:array, [:str, ""]]]]]]]]],

  "def evaluate rule; foo; bar;rescue Exception;    puts \"\";\
  end" =>
    [[:defn, :evaluate, [:scope, [:block, [:args, :rule], [
    :rescue, [:block, [:vcall, :foo], [:vcall, :bar]], [:resbody, [
    :array, [:const, :Exception]], [:fcall, :puts, [:array, [
    :str, ""]]]]]]]]],

  "def evaluate rule; foo; bar;rescue Exception=>e;    puts e;\
  end" =>
    [[:defn, :evaluate, [:scope, [:block, [:args, :rule], [
    :rescue, [:block, [:vcall, :foo], [:vcall, :bar]], [:resbody, [
    :array, [:const, :Exception]], [:block, [:lasgn, :e, [:gvar,
     :$!]], [:fcall, :puts, [:array, [:lvar, :e]]]]]]]]]],

  "def evaluate rule; foo; bar;rescue RX; f(g); rescue Exception=>e;\
    puts e;  else k end" =>
    [[:defn, :evaluate, [:scope, [:block, [:args, :rule], [
    :rescue, [:block, [:vcall, :foo], [:vcall, :bar]], [:resbody, [
    :array, [:const, :RX]], [:fcall, :f, [:array, [:vcall, :g]]
    ], [:resbody, [:array, [:const, :Exception]], [:block, [
    :lasgn, :e, [:gvar, :$!]], [:fcall, :puts, [:array, [:lvar,
     :e]]]]]], [:vcall, :k]]]]]],

  "def evaluate rule; foo; bar;rescue RX; f(g); rescue Exception=>e;\
    puts e;  end" =>
    [[:defn, :evaluate, [:scope, [:block, [:args, :rule], [
    :rescue, [:block, [:vcall, :foo], [:vcall, :bar]], [:resbody, [
    :array, [:const, :RX]], [:fcall, :f, [:array, [:vcall, :g]]
    ], [:resbody, [:array, [:const, :Exception]], [:block, [
    :lasgn, :e, [:gvar, :$!]], [:fcall, :puts, [:array, [:lvar,
     :e]]]]]]]]]]],

  "def evaluate rule; foo; rescue Exception;    puts \"\";  end\
" =>
    [[:defn, :evaluate, [:scope, [:block, [:args, :rule], [
    :rescue, [:vcall, :foo], [:resbody, [:array, [:const, :Exception]], [
    :fcall, :puts, [:array, [:str, ""]]]]]]]]],

  "def evaluate rule; foo; rescue Exception=>e;    puts e;  end\
" =>
    [[:defn, :evaluate, [:scope, [:block, [:args, :rule], [
    :rescue, [:vcall, :foo], [:resbody, [:array, [:const, :Exception]], [
    :block, [:lasgn, :e, [:gvar, :$!]], [:fcall, :puts, [:array,
     [:lvar, :e]]]]]]]]]],

  "def evaluate rule; foo; rescue RX; f(g); rescue Exception=>e;\
    puts e;  else k end" =>
    [[:defn, :evaluate, [:scope, [:block, [:args, :rule], [
    :rescue, [:vcall, :foo], [:resbody, [:array, [:const, :RX]],
     [:fcall, :f, [:array, [:vcall, :g]]], [:resbody, [:array, [
    :const, :Exception]], [:block, [:lasgn, :e, [:gvar, :$!]], [
    :fcall, :puts, [:array, [:lvar, :e]]]]]], [:vcall, :k]]]]]],

  "def evaluate rule; foo; rescue RX; f(g); rescue Exception=>e;\
    puts e;  end" =>
    [[:defn, :evaluate, [:scope, [:block, [:args, :rule], [
    :rescue, [:vcall, :foo], [:resbody, [:array, [:const, :RX]],
     [:fcall, :f, [:array, [:vcall, :g]]], [:resbody, [:array, [
    :const, :Exception]], [:block, [:lasgn, :e, [:gvar, :$!]], [
    :fcall, :puts, [:array, [:lvar, :e]]]]]]]]]]],

  "def f(a,b,*); c end" =>
    [[:defn, :f, [:scope, [:block, [:args, :a, :b, :*], [:vcall,
     :c]]]]],

  "def false.x; end" => [[:defs, [:false], :x, [:scope, [:args]]]],
  "def false; end" =>
    [[:defn, :false, [:scope, [:block, [:args], [:nil]]]]],

  "def foo(a = 1)    end; def foo(a=b=c={})  end; def bar(a=b=\
c=1,d=2)  end" =>
    [[:block, [:defn, :foo, [:scope, [:block, [:args, :a, [:block, [
    :lasgn, :a, [:lit, 1]]]], [:nil]]]], [:defn, :foo, [:scope, [
    :block, [:args, :b, [:block, [:lasgn, :a, [:lasgn, :b, [
    :lasgn, :c, [:hash]]]]]], [:nil]]]], [:defn, :bar, [:scope, [
    :block, [:args, :b, :c, [:block, [:lasgn, :a, [:lasgn, :b, [
    :lasgn, :c, [:lit, 1]]]], [:lasgn, :d, [:lit, 2]]]], [:nil]]
    ]]]],

  "\
def foo(a=<<a,b=<<b,c=<<c)
jfksdkjf
dkljjkf
a
kdljfjkdg
dfglkdfkgjdf
dkf
b
lkdffdjksadhf
sdflkdjgsfdkjgsdg
dsfg;lkdflisgffd
g
c
   a+b+c
end\

" =>
    [[:defn, :foo, [:scope, [:block, [:args, :a, :b, :c, [:block, [
    :lasgn, :a, [:str, "jfksdkjf\ndkljjkf\n"]], [:lasgn, :b, [
    :str, "kdljfjkdg\ndfglkdfkgjdf\ndkf\n"]], [:lasgn, :c, [:str,
     "lkdffdjksadhf\nsdflkdjgsfdkjgsdg\ndsfg;lkdflisgffd\ng\n"]]
    ]], [:call, [:call, [:lvar, :a], :+, [:array, [:lvar, :b]]],
     :+, [:array, [:lvar, :c]]]]]]],

  "def foo(a=b=c={}) end" =>
    [[:defn, :foo, [:scope, [:block, [:args, :b, [:block, [
    :lasgn, :a, [:lasgn, :b, [:lasgn, :c, [:hash]]]]]], [:nil]]]
    ]],

  "def foo(bar=5,tt=6) end" =>
    [[:defn, :foo, [:scope, [:block, [:args, :bar, :tt, [:block,
     [:lasgn, :bar, [:lit, 5]], [:lasgn, :tt, [:lit, 6]]]], [
    :nil]]]]],

  "\
def foo1
  p (1..10).method(:each)    #implicit parens around the whole thing
end\

" =>
    [[:defn, :foo1, [:scope, [:block, [:args], [:fcall, :p, [
    :array, [:call, [:lit, 1..10], :method, [:array, [:lit, :each]
    ]]]]]]],
    {:warnings=>["(string):2: warning: (...) interpreted as grouped expression"]}],

  "\
def foo2()
  p((1..10).method(:each))  #explicitly parenthesized... no implicit parens needed
end\

" =>
    [[:defn, :foo2, [:scope, [:block, [:args], [:fcall, :p, [
    :array, [:call, [:lit, 1..10], :method, [:array, [:lit, :each]
    ]]]]]]]],

  "\
def foo3()
  p (1..10).method(:each)   #implicit parens around the whole thing
end\

" =>
    [[:defn, :foo3, [:scope, [:block, [:args], [:fcall, :p, [
    :array, [:call, [:lit, 1..10], :method, [:array, [:lit, :each]
    ]]]]]]],
    {:warnings=>["(string):2: warning: (...) interpreted as grouped expression"]}],

  "\
def foo; end
undef
foo\

" =>
    [[:block, [:defn, :foo, [:scope, [:block, [:args], [:nil]]]
    ], [:undef, [:lit, :foo]]]],

  "def foo; return{a=>b} end" =>
    [[:defn, :foo, [:scope, [:block, [:args], [:return, [:hash, [
    :vcall, :a], [:vcall, :b]]]]]]],

  "def foo; return{} end" =>
    [[:defn, :foo, [:scope, [:block, [:args], [:return, [:hash]]
    ]]]],

  "\
def foobar() end
def foobar2
end\

" =>
    [[:block, [:defn, :foobar, [:scope, [:block, [:args], [:nil]
    ]]], [:defn, :foobar2, [:scope, [:block, [:args], [:nil]]]]]
    ],

  "def for; end" =>
    [[:defn, :for, [:scope, [:block, [:args], [:nil]]]]],

  "\
def ggg(x=nil) p x;9 end
 (ggg / 1)\

" =>
    [[:block, [:defn, :ggg, [:scope, [:block, [:args, :x, [:block, [
    :lasgn, :x, [:nil]]]], [:fcall, :p, [:array, [:lvar, :x]]], [
    :lit, 9]]]], [:call, [:vcall, :ggg], :/, [:array, [:lit, 1]]
    ]]],

  "def i;\"..\#{@@c = 1}\";end" =>
    [[:defn, :i, [:scope, [:block, [:args], [:dstr, "..", [:evstr, [
    :cvasgn, :@@c, [:lit, 1]]]]]]]],

  "def if; end" =>
    [[:defn, :if, [:scope, [:block, [:args], [:nil]]]]],

  "def in; end" =>
    [[:defn, :in, [:scope, [:block, [:args], [:nil]]]]],

  "def intialize(resolvers=[Hosts.new, DNS.new]) end" =>
    [[:defn, :intialize, [:scope, [:block, [:args, :resolvers, [
    :block, [:lasgn, :resolvers, [:array, [:call, [:const, :Hosts],
     :new], [:call, [:const, :DNS], :new]]]]], [:nil]]]]],

  "def jd_to_wday(jd) (jd + 1) % 7 end" =>
    [[:defn, :jd_to_wday, [:scope, [:block, [:args, :jd], [:call, [
    :call, [:lvar, :jd], :+, [:array, [:lit, 1]]], :%, [:array, [
    :lit, 7]]]]]]],

  "def l; m; rescue begin; a; rescue b; end,o:  end" =>
    [[:defn, :l, [:scope, [:block, [:args], [:rescue, [:vcall, :m], [
    :resbody, [:array, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]], [:vcall, :o]]]]]]]],

  "def l; m; rescue begin; a; rescue b; end:  end" =>
    [[:defn, :l, [:scope, [:block, [:args], [:rescue, [:vcall, :m], [
    :resbody, [:array, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]]]]]]]]],

  "def line(host = host,guest=host); end" =>
    [[:defn, :line, [:scope, [:block, [:args, :host, :guest, [
    :block, [:lasgn, :host, [:vcall, :host]], [:lasgn, :guest, [
    :lvar, :host]]]], [:nil]]]]],

  "def line(type, txt, selector, host = host, port = port); end\
" =>
    [[:defn, :line, [:scope, [:block, [:args, :type, :txt, :selector,
     :host, :port, [:block, [:lasgn, :host, [:vcall, :host]], [
    :lasgn, :port, [:vcall, :port]]]], [:nil]]]]],

  "def m n=begin; a; rescue b; end; end" =>
    [[:defn, :m, [:scope, [:block, [:args, :n, [:block, [:lasgn,
     :n, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall, :b]
    ]]]]]], [:nil]]]]],

  "def maybe(chance = 0.5)end" =>
    [[:defn, :maybe, [:scope, [:block, [:args, :chance, [:block,
     [:lasgn, :chance, [:lit, 0.5]]]], [:nil]]]]],

  "def mo4(a, *b, &c) end" =>
    [[:defn, :mo4, [:scope, [:block, [:args, :a, :"*b"], [
    :block_arg, :c], [:nil]]]]],

  "def module; end" =>
    [[:defn, :module, [:scope, [:block, [:args], [:nil]]]]],

  "def next.x; end" => SyntaxError.new("(string):1: syntax error, unexpected '.', expecting '\\n' or ';'\ndef next.x; end\n         ^\n(string):1: syntax error, unexpected kEND, expecting $end\ndef next.x; end\n               ^"),
  "def next; end" =>
    [[:defn, :next, [:scope, [:block, [:args], [:nil]]]]],

  "def nil.+(x) ~x end" =>
    [[:defs, [:nil], :+, [:scope, [:block, [:args, :x], [:call, [
    :lvar, :x], :~]]]]],

  "def nil.[](*x) [x] end" =>
    [[:defs, [:nil], :[], [:scope, [:block, [:args, :"*x"], [
    :array, [:lvar, :x]]]]]],

  "def nil.x; end" => [[:defs, [:nil], :x, [:scope, [:args]]]],
  "def nil; end" =>
    [[:defn, :nil, [:scope, [:block, [:args], [:nil]]]]],

  "\
def params_quoted(field_name, default)
  if block_given? then yield field_name else default end
end\

" =>
    [[:defn, :params_quoted, [:scope, [:block, [:args, :field_name,
     :default], [:if, [:fcall, :block_given?], [:yield, [:lvar,
     :field_name]], [:lvar, :default]]]]]],

  "\
def printem a,b,c
   p a;p b;p c
   p(a +77)
   p(b %(0.123))
end
printem 1,2,3\

" =>
    [[:block, [:defn, :printem, [:scope, [:block, [:args, :a, :b,
     :c], [:fcall, :p, [:array, [:lvar, :a]]], [:fcall, :p, [
    :array, [:lvar, :b]]], [:fcall, :p, [:array, [:lvar, :c]]], [
    :fcall, :p, [:array, [:call, [:lvar, :a], :+, [:array, [:lit,
     77]]]]], [:fcall, :p, [:array, [:call, [:lvar, :b], :%, [
    :array, [:lit, 0.123]]]]]]]], [:fcall, :printem, [:array, [
    :lit, 1], [:lit, 2], [:lit, 3]]]]],

  "\
def printem0(a)
   p(a +77)
end\

" =>
    [[:defn, :printem0, [:scope, [:block, [:args, :a], [:fcall,
     :p, [:array, [:call, [:lvar, :a], :+, [:array, [:lit, 77]]]
    ]]]]]],

  "\
def printem1 a,b,c
   p(a +77)
   p(b +77)
   p(c +77)
end\

" =>
    [[:defn, :printem1, [:scope, [:block, [:args, :a, :b, :c], [
    :fcall, :p, [:array, [:call, [:lvar, :a], :+, [:array, [:lit,
     77]]]]], [:fcall, :p, [:array, [:call, [:lvar, :b], :+, [
    :array, [:lit, 77]]]]], [:fcall, :p, [:array, [:call, [:lvar,
     :c], :+, [:array, [:lit, 77]]]]]]]]],

  "\
def printem10 a,b,c,d,e,f
   p(a +77)
   p(b +77)
   p(c +77)
   p(d +77)
   p(e +77)
   p(f +77)
end\

" =>
    [[:defn, :printem10, [:scope, [:block, [:args, :a, :b, :c,
     :d, :e, :f], [:fcall, :p, [:array, [:call, [:lvar, :a], :+,
     [:array, [:lit, 77]]]]], [:fcall, :p, [:array, [:call, [
    :lvar, :b], :+, [:array, [:lit, 77]]]]], [:fcall, :p, [:array, [
    :call, [:lvar, :c], :+, [:array, [:lit, 77]]]]], [:fcall, :p, [
    :array, [:call, [:lvar, :d], :+, [:array, [:lit, 77]]]]], [
    :fcall, :p, [:array, [:call, [:lvar, :e], :+, [:array, [:lit,
     77]]]]], [:fcall, :p, [:array, [:call, [:lvar, :f], :+, [
    :array, [:lit, 77]]]]]]]]],

  "\
def printem100 a,b,c,d,e,f,*g,&h
   p(a +77)
   p(b +77)
   p(c +77)
   p(d +77)
   p(e +77)
   p(f +77)
   p(g +77)
   p(h +77)
end\

" =>
    [[:defn, :printem100, [:scope, [:block, [:args, :a, :b, :c,
     :d, :e, :f, :"*g"], [:block_arg, :h], [:fcall, :p, [:array,
     [:call, [:lvar, :a], :+, [:array, [:lit, 77]]]]], [:fcall,
     :p, [:array, [:call, [:lvar, :b], :+, [:array, [:lit, 77]]]
    ]], [:fcall, :p, [:array, [:call, [:lvar, :c], :+, [:array, [
    :lit, 77]]]]], [:fcall, :p, [:array, [:call, [:lvar, :d], :+, [
    :array, [:lit, 77]]]]], [:fcall, :p, [:array, [:call, [:lvar,
     :e], :+, [:array, [:lit, 77]]]]], [:fcall, :p, [:array, [
    :call, [:lvar, :f], :+, [:array, [:lit, 77]]]]], [:fcall, :p, [
    :array, [:call, [:lvar, :g], :+, [:array, [:lit, 77]]]]], [
    :fcall, :p, [:array, [:call, [:lvar, :h], :+, [:array, [:lit,
     77]]]]]]]]],

  "def printem2 a,b,c; p(a +77); p(b +77); p(c +77) end" =>
    [[:defn, :printem2, [:scope, [:block, [:args, :a, :b, :c], [
    :fcall, :p, [:array, [:call, [:lvar, :a], :+, [:array, [:lit,
     77]]]]], [:fcall, :p, [:array, [:call, [:lvar, :b], :+, [
    :array, [:lit, 77]]]]], [:fcall, :p, [:array, [:call, [:lvar,
     :c], :+, [:array, [:lit, 77]]]]]]]]],

  "\
def printem_1 a,b
   p(a +77)
   p(b +77)
end\

" =>
    [[:defn, :printem_1, [:scope, [:block, [:args, :a, :b], [
    :fcall, :p, [:array, [:call, [:lvar, :a], :+, [:array, [:lit,
     77]]]]], [:fcall, :p, [:array, [:call, [:lvar, :b], :+, [
    :array, [:lit, 77]]]]]]]]],

  "\
def printem__1 a
   p(a +77)
end\

" =>
    [[:defn, :printem__1, [:scope, [:block, [:args, :a], [:fcall,
     :p, [:array, [:call, [:lvar, :a], :+, [:array, [:lit, 77]]]
    ]]]]]],

  "\
def printem___1 a
   a
end\

" =>
    [[:defn, :printem___1, [:scope, [:block, [:args, :a], [:lvar,
     :a]]]]],

  "def r;4 end; r=r.nil? " =>
    [[:block, [:defn, :r, [:scope, [:block, [:args], [:lit, 4]]]
    ], [:lasgn, :r, [:call, [:lvar, :r], :nil?]]]],

  "def raise.x; end" =>
    [[:defs, [:vcall, :raise], :x, [:scope, [:args]]]],

  "def raise; end" =>
    [[:defn, :raise, [:scope, [:block, [:args], [:nil]]]]],

  "def rescue; end" =>
    [[:defn, :rescue, [:scope, [:block, [:args], [:nil]]]]],

  "def return.x; end" => SyntaxError.new("(string):1: syntax error, unexpected '.', expecting '\\n' or ';'\ndef return.x; end\n           ^\n(string):1: syntax error, unexpected kEND, expecting $end\ndef return.x; end\n                 ^"),
  "def return; end" =>
    [[:defn, :return, [:scope, [:block, [:args], [:nil]]]]],

  "def run; system; else;end" =>
    [[:defn, :run, [:scope, [:block, [:args], [:vcall, :system]]
    ]]],

  "def self.defin(foo) () end" =>
    [[:defs, [:self], :defin, [:scope, [:block, [:args, :foo], [
    :nil]]]]],

  "def self.defin(foo) [] end" =>
    [[:defs, [:self], :defin, [:scope, [:block, [:args, :foo], [
    :zarray]]]]],

  "def self.defin(foo) end" =>
    [[:defs, [:self], :defin, [:scope, [:args, :foo]]]],

  "def self.defin(foo) hh end" =>
    [[:defs, [:self], :defin, [:scope, [:block, [:args, :foo], [
    :vcall, :hh]]]]],

  "def self.defin(foo) nil end" =>
    [[:defs, [:self], :defin, [:scope, [:block, [:args, :foo], [
    :nil]]]]],

  "def self.defin(foo) {} end" =>
    [[:defs, [:self], :defin, [:scope, [:block, [:args, :foo], [
    :hash]]]]],

  "def self.defin?(foo) () end" =>
    [[:defs, [:self], :defin?, [:scope, [:block, [:args, :foo], [
    :nil]]]]],

  "def self.defin?(foo) [] end" =>
    [[:defs, [:self], :defin?, [:scope, [:block, [:args, :foo], [
    :zarray]]]]],

  "def self.defin?(foo) end" =>
    [[:defs, [:self], :defin?, [:scope, [:args, :foo]]]],

  "def self.defin?(foo) hh end" =>
    [[:defs, [:self], :defin?, [:scope, [:block, [:args, :foo], [
    :vcall, :hh]]]]],

  "def self.defin?(foo) nil end" =>
    [[:defs, [:self], :defin?, [:scope, [:block, [:args, :foo], [
    :nil]]]]],

  "def self.defin?(foo) {} end" =>
    [[:defs, [:self], :defin?, [:scope, [:block, [:args, :foo], [
    :hash]]]]],

  "def self.defined?(foo) () end" =>
    [[:defs, [:self], :defined?, [:scope, [:block, [:args, :foo], [
    :nil]]]]],

  "def self.defined?(foo) [] end" =>
    [[:defs, [:self], :defined?, [:scope, [:block, [:args, :foo], [
    :zarray]]]]],

  "def self.defined?(foo) end" =>
    [[:defs, [:self], :defined?, [:scope, [:args, :foo]]]],

  "def self.defined?(foo) hh end" =>
    [[:defs, [:self], :defined?, [:scope, [:block, [:args, :foo], [
    :vcall, :hh]]]]],

  "def self.defined?(foo) nil end" =>
    [[:defs, [:self], :defined?, [:scope, [:block, [:args, :foo], [
    :nil]]]]],

  "def self.defined?(foo) {} end" =>
    [[:defs, [:self], :defined?, [:scope, [:block, [:args, :foo], [
    :hash]]]]],

  "def self.x; end" => [[:defs, [:self], :x, [:scope, [:args]]]],
  "def self.z(*); end" =>
    [[:defs, [:self], :z, [:scope, [:args, :*]]]],

  "def self.zz(&x) y end" =>
    [[:defs, [:self], :zz, [:scope, [:block, [:args], [:block_arg,
     :x], [:vcall, :y]]]]],

  "def self.zz(&x); end" =>
    [[:defs, [:self], :zz, [:scope, [:block, [:args], [:block_arg,
     :x]]]]],

  "def self.zz(*,&x) y end" =>
    [[:defs, [:self], :zz, [:scope, [:block, [:args, :*], [
    :block_arg, :x], [:vcall, :y]]]]],

  "def self.zz(*,&x); end" =>
    [[:defs, [:self], :zz, [:scope, [:block, [:args, :*], [
    :block_arg, :x]]]]],

  "def self.zzz(&x); end" =>
    [[:defs, [:self], :zzz, [:scope, [:block, [:args], [
    :block_arg, :x]]]]],

  "def self; end" =>
    [[:defn, :self, [:scope, [:block, [:args], [:nil]]]]],

  "def st;  begin ire end end" =>
    [[:defn, :st, [:scope, [:block, [:args], [:vcall, :ire]]]]],

  "def subject::Builder.foo; bar end" => SyntaxError.new("(string):1: syntax error, unexpected '.', expecting '\\n' or ';'\ndef subject::Builder.foo; bar end\n                     ^\n(string):1: syntax error, unexpected kEND, expecting $end\ndef subject::Builder.foo; bar end\n                                 ^"),
  "def sum(options = {:weights => weights = Hash.new(1)});\
 options.empty? or options.keys.size > 1; end" =>
    [[:defn, :sum, [:scope, [:block, [:args, :weights, [:block, [
    :lasgn, :options, [:hash, [:lit, :weights], [:lasgn, :weights, [
    :call, [:const, :Hash], :new, [:array, [:lit, 1]]]]]]]], [
    :or, [:call, [:lvar, :options], :empty?], [:call, [:call, [
    :call, [:lvar, :options], :keys], :size], :>, [:array, [:lit,
     1]]]]]]]],

  "def sum(options = {:weights => weights = Hash}); 1 end" =>
    [[:defn, :sum, [:scope, [:block, [:args, :weights, [:block, [
    :lasgn, :options, [:hash, [:lit, :weights], [:lasgn, :weights, [
    :const, :Hash]]]]]], [:lit, 1]]]]],

  "def three() (1+2) end" =>
    [[:defn, :three, [:scope, [:block, [:args], [:call, [:lit,
     1], :+, [:array, [:lit, 2]]]]]]],

  "def throw.x; end" =>
    [[:defs, [:vcall, :throw], :x, [:scope, [:args]]]],

  "def throw; end" =>
    [[:defn, :throw, [:scope, [:block, [:args], [:nil]]]]],

  "def true.x; end" => [[:defs, [:true], :x, [:scope, [:args]]]],
  "def true; end" =>
    [[:defn, :true, [:scope, [:block, [:args], [:nil]]]]],

  "def tt3(&b) end" =>
    [[:defn, :tt3, [:scope, [:block, [:args], [:block_arg, :b], [
    :nil]]]]],

  "def tt3(&b) tt2(&b) end" =>
    [[:defn, :tt3, [:scope, [:block, [:args], [:block_arg, :b], [
    :block_pass, [:lvar, :b], [:fcall, :tt2]]]]]],

  "def undef; end" =>
    [[:defn, :undef, [:scope, [:block, [:args], [:nil]]]]],

  "def unless; end" =>
    [[:defn, :unless, [:scope, [:block, [:args], [:nil]]]]],

  "def until; end" =>
    [[:defn, :until, [:scope, [:block, [:args], [:nil]]]]],

  "def v; t; else return ([vUew]).e  end" =>
    [[:defn, :v, [:scope, [:block, [:args], [:vcall, :t], [
    :return, [:call, [:array, [:vcall, :vUew]], :e]]]]],
    {:warnings=>["(string):1: warning: else without rescue is useless"]}],

  "def wait; begin; ync; mup; end ;end" =>
    [[:defn, :wait, [:scope, [:block, [:args], [:block, [:vcall,
     :ync], [:vcall, :mup]]]]]],

  "def while; end" =>
    [[:defn, :while, [:scope, [:block, [:args], [:nil]]]]],

  "def x.defin(foo) () end" =>
    [[:defs, [:vcall, :x], :defin, [:scope, [:block, [:args, :foo], [
    :nil]]]]],

  "def x.defin(foo) [] end" =>
    [[:defs, [:vcall, :x], :defin, [:scope, [:block, [:args, :foo], [
    :zarray]]]]],

  "def x.defin(foo) end" =>
    [[:defs, [:vcall, :x], :defin, [:scope, [:args, :foo]]]],

  "def x.defin(foo) hh end" =>
    [[:defs, [:vcall, :x], :defin, [:scope, [:block, [:args, :foo], [
    :vcall, :hh]]]]],

  "def x.defin(foo) nil end" =>
    [[:defs, [:vcall, :x], :defin, [:scope, [:block, [:args, :foo], [
    :nil]]]]],

  "def x.defin(foo) {} end" =>
    [[:defs, [:vcall, :x], :defin, [:scope, [:block, [:args, :foo], [
    :hash]]]]],

  "def x.defin?(foo) () end" =>
    [[:defs, [:vcall, :x], :defin?, [:scope, [:block, [:args, :foo], [
    :nil]]]]],

  "def x.defin?(foo) [] end" =>
    [[:defs, [:vcall, :x], :defin?, [:scope, [:block, [:args, :foo], [
    :zarray]]]]],

  "def x.defin?(foo) end" =>
    [[:defs, [:vcall, :x], :defin?, [:scope, [:args, :foo]]]],

  "def x.defin?(foo) hh end" =>
    [[:defs, [:vcall, :x], :defin?, [:scope, [:block, [:args, :foo], [
    :vcall, :hh]]]]],

  "def x.defin?(foo) nil end" =>
    [[:defs, [:vcall, :x], :defin?, [:scope, [:block, [:args, :foo], [
    :nil]]]]],

  "def x.defin?(foo) {} end" =>
    [[:defs, [:vcall, :x], :defin?, [:scope, [:block, [:args, :foo], [
    :hash]]]]],

  "def x.defined?(foo) () end" =>
    [[:defs, [:vcall, :x], :defined?, [:scope, [:block, [:args,
     :foo], [:nil]]]]],

  "def x.defined?(foo) [] end" =>
    [[:defs, [:vcall, :x], :defined?, [:scope, [:block, [:args,
     :foo], [:zarray]]]]],

  "def x.defined?(foo) end" =>
    [[:defs, [:vcall, :x], :defined?, [:scope, [:args, :foo]]]],

  "def x.defined?(foo) hh end" =>
    [[:defs, [:vcall, :x], :defined?, [:scope, [:block, [:args,
     :foo], [:vcall, :hh]]]]],

  "def x.defined?(foo) nil end" =>
    [[:defs, [:vcall, :x], :defined?, [:scope, [:block, [:args,
     :foo], [:nil]]]]],

  "def x.defined?(foo) {} end" =>
    [[:defs, [:vcall, :x], :defined?, [:scope, [:block, [:args,
     :foo], [:hash]]]]],

  "def x; yield end #this must be first!!!!" =>
    [[:defn, :x, [:scope, [:block, [:args], [:yield]]]]],

  "\
def x; yield end #this must be first!!!!
#the purpose of x{...} is to prevent the enclosed code from
#modifying the list of known local variables. it may be omitted
#in cases where it is known that no local vars are defined.\

" =>
    [[:defn, :x, [:scope, [:block, [:args], [:yield]]]]],

  "def yield.x; end" => SyntaxError.new("(string):1: syntax error, unexpected '.', expecting '\\n' or ';'\ndef yield.x; end\n          ^\n(string):1: syntax error, unexpected kEND, expecting $end\ndef yield.x; end\n                ^"),
  "def yield; end" =>
    [[:defn, :yield, [:scope, [:block, [:args], [:nil]]]]],

  "def ~@; :foo end" =>
    [[:defn, :~, [:scope, [:block, [:args], [:lit, :foo]]]]],

  "def ~@; end" =>
    [[:defn, :~, [:scope, [:block, [:args], [:nil]]]]],

  "defined? %/f/" => [[:defined, [:str, "f"]]],
  "defined? * @com_disk" => SyntaxError.new("(string):1: syntax error, unexpected '*'\ndefined? * @com_disk\n          ^"),
  "defined? +1" =>
    [[:defined, [:lit, 1]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "defined? +1.0" =>
    [[:defined, [:lit, 1.0]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "defined? - @com_disk" => SyntaxError.new("(string):1: syntax error, unexpected '-'\ndefined? - @com_disk\n          ^"),
  "defined? -1" =>
    [[:defined, [:lit, -1]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "defined? -1.0" =>
    [[:defined, [:lit, -1.0]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "defined? ::A" => [[:defined, [:colon3, :A]]],
  "defined? ?A" => [[:defined, [:lit, 65]]],
  "defined? MOD_RUBY && ENV" =>
    [[:defined, [:and, [:const, :MOD_RUBY], [:const, :ENV]]]],

  "defined? MOD_RUBY and ENV" =>
    [[:and, [:defined, [:const, :MOD_RUBY]], [:const, :ENV]]],

  "defined? MOD_RUBY or ENV" =>
    [[:or, [:defined, [:const, :MOD_RUBY]], [:const, :ENV]]],

  "defined? MOD_RUBY || ENV" =>
    [[:defined, [:or, [:const, :MOD_RUBY], [:const, :ENV]]]],

  "defined? MOD_RUBY!=ENV" =>
    [[:defined, [:not, [:call, [:const, :MOD_RUBY], :==, [:array, [
    :const, :ENV]]]]]],

  "defined? MOD_RUBY%=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:call, [:const, :MOD_RUBY],
     :%, [:array, [:const, :ENV]]]]]],

  "defined? MOD_RUBY%ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :%, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY&&=ENV" =>
    [[:defined, [:op_asgn_and, [:const, :MOD_RUBY], [:cdecl, :MOD_RUBY, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY&=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:call, [:const, :MOD_RUBY],
     :&, [:array, [:const, :ENV]]]]]],

  "defined? MOD_RUBY&ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :&, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY**=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:call, [:const, :MOD_RUBY],
     :**, [:array, [:const, :ENV]]]]]],

  "defined? MOD_RUBY**ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :**, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY*=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:call, [:const, :MOD_RUBY],
     :*, [:array, [:const, :ENV]]]]]],

  "defined? MOD_RUBY*ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :*, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY+=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:call, [:const, :MOD_RUBY],
     :+, [:array, [:const, :ENV]]]]]],

  "defined? MOD_RUBY+ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :+, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY-=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:call, [:const, :MOD_RUBY],
     :-, [:array, [:const, :ENV]]]]]],

  "defined? MOD_RUBY-ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :-, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY...ENV" =>
    [[:defined, [:dot3, [:const, :MOD_RUBY], [:const, :ENV]]]],

  "defined? MOD_RUBY..ENV" =>
    [[:defined, [:dot2, [:const, :MOD_RUBY], [:const, :ENV]]]],

  "defined? MOD_RUBY.ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :ENV]]],

  "defined? MOD_RUBY/=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:call, [:const, :MOD_RUBY],
     :/, [:array, [:const, :ENV]]]]]],

  "defined? MOD_RUBY/ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :/, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY::ENV" =>
    [[:defined, [:colon2, [:const, :MOD_RUBY], :ENV]]],

  "defined? MOD_RUBY<<=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:call, [:const, :MOD_RUBY],
     :<<, [:array, [:const, :ENV]]]]]],

  "defined? MOD_RUBY<<ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :<<, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY<=>ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :<=>, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY<=ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :<=, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY<ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :<, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY===ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :===, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY==ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :==, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:const, :ENV]]]],

  "defined? MOD_RUBY=~ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :=~, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY>=ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :>=, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY>>=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:call, [:const, :MOD_RUBY],
     :>>, [:array, [:const, :ENV]]]]]],

  "defined? MOD_RUBY>>ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :>>, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY>ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :>, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY[ENV]" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :[], [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY[ENV]=1" =>
    [[:defined, [:attrasgn, [:const, :MOD_RUBY], :[]=, [:array, [
    :const, :ENV], [:lit, 1]]]]],

  "defined? MOD_RUBY^=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:call, [:const, :MOD_RUBY],
     :^, [:array, [:const, :ENV]]]]]],

  "defined? MOD_RUBY^ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :^, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY|=ENV" =>
    [[:defined, [:cdecl, :MOD_RUBY, [:call, [:const, :MOD_RUBY],
     :|, [:array, [:const, :ENV]]]]]],

  "defined? MOD_RUBY|ENV" =>
    [[:defined, [:call, [:const, :MOD_RUBY], :|, [:array, [
    :const, :ENV]]]]],

  "defined? MOD_RUBY||=ENV" =>
    [[:defined, [:op_asgn_or, [:const, :MOD_RUBY], [:cdecl, :MOD_RUBY, [
    :const, :ENV]]]]],

  "defined? []" => [[:defined, [:zarray]]],
  "defined? a" => [[:defined, [:vcall, :a]]],
  "defined? a rescue b" =>
    [[:rescue, [:defined, [:vcall, :a]], [:resbody, nil, [:vcall,
     :b]]]],

  "defined? ~1" => [[:defined, [:call, [:lit, 1], :~]]],
  "defined?(BINARY) ? BINARY : 0" =>
    [[:if, [:defined, [:const, :BINARY]], [:const, :BINARY], [
    :lit, 0]]],

  "defined?(MOD_RUBY) && ENV" =>
    [[:and, [:defined, [:const, :MOD_RUBY]], [:const, :ENV]]],

  "defined?(MOD_RUBY) and ENV" =>
    [[:and, [:defined, [:const, :MOD_RUBY]], [:const, :ENV]]],

  "defined?(MOD_RUBY) or ENV" =>
    [[:or, [:defined, [:const, :MOD_RUBY]], [:const, :ENV]]],

  "defined?(MOD_RUBY) || ENV" =>
    [[:or, [:defined, [:const, :MOD_RUBY]], [:const, :ENV]]],

  "defined?(MOD_RUBY)!=ENV" =>
    [[:not, [:call, [:defined, [:const, :MOD_RUBY]], :==, [:array, [
    :const, :ENV]]]]],

  "defined?(MOD_RUBY)!~ENV" =>
    [[:not, [:call, [:defined, [:const, :MOD_RUBY]], :=~, [:array, [
    :const, :ENV]]]]],

  "defined?(MOD_RUBY)%ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :%, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)&ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :&, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)**ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :**, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)*ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :*, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)+ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :+, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)-ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :-, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)...ENV" =>
    [[:dot3, [:defined, [:const, :MOD_RUBY]], [:const, :ENV]]],

  "defined?(MOD_RUBY)..ENV" =>
    [[:dot2, [:defined, [:const, :MOD_RUBY]], [:const, :ENV]]],

  "defined?(MOD_RUBY).ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :ENV]],

  "defined?(MOD_RUBY)/ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :/, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)::ENV" =>
    [[:colon2, [:defined, [:const, :MOD_RUBY]], :ENV]],

  "defined?(MOD_RUBY)<<ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :<<, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)<=>ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :<=>, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)<=ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :<=, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)<ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :<, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)===ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :===, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)==ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :==, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)=~ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :=~, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)>=ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :>=, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)>>ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :>>, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)>ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :>, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)[ENV]" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :[], [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)[ENV]=1" =>
    [[:attrasgn, [:defined, [:const, :MOD_RUBY]], :[]=, [:array,
     [:const, :ENV], [:lit, 1]]]],

  "defined?(MOD_RUBY)^ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :^, [:array, [
    :const, :ENV]]]],

  "defined?(MOD_RUBY)|ENV" =>
    [[:call, [:defined, [:const, :MOD_RUBY]], :|, [:array, [
    :const, :ENV]]]],

  "defined?({})" => [[:defined, [:hash]]],
  "doc_status, err_args = Documeh_status{fcgi_state = 3; docespond\
 do doc_response =fcgi_state =  1; end }" =>
    [[:masgn, [:array, [:lasgn, :doc_status], [:lasgn, :err_args]
    ], nil, [:to_ary, [:iter, [:fcall, :Documeh_status], nil, [
    :block, [:dasgn_curr, :fcgi_state], [:dasgn_curr, :fcgi_state, [
    :lit, 3]], [:iter, [:fcall, :docespond], nil, [:dasgn_curr,
     :doc_response, [:dasgn, :fcgi_state, [:lit, 1]]]]]]]]],

  "e if f rescue 0" =>
    [[:rescue, [:if, [:vcall, :f], [:vcall, :e], nil], [:resbody,
     nil, [:lit, 0]]]],

  "e unless f rescue 0" =>
    [[:rescue, [:if, [:vcall, :f], nil, [:vcall, :e]], [:resbody,
     nil, [:lit, 0]]]],

  "e until f rescue 0" =>
    [[:rescue, [:until, [:vcall, :f], [:vcall, :e], true], [
    :resbody, nil, [:lit, 0]]]],

  "e while f rescue 0" =>
    [[:rescue, [:while, [:vcall, :f], [:vcall, :e], true], [
    :resbody, nil, [:lit, 0]]]],

  "e { |c|; print \"%02X\" % c }" =>
    [[:iter, [:fcall, :e], [:dasgn_curr, :c], [:fcall, :print, [
    :array, [:call, [:str, "%02X"], :%, [:array, [:dvar, :c]]]]]
    ]],

  "end * @com_disk" => SyntaxError.new("(string):1: syntax error, unexpected kEND\nend * @com_disk\n   ^"),
  "end - @com_disk" => SyntaxError.new("(string):1: syntax error, unexpected kEND\nend - @com_disk\n   ^"),
  "eof ?n" => [[:fcall, :eof, [:array, [:lit, 110]]]],
  "eof ?ni : 0" =>
    [[:if, [:vcall, :eof], [:vcall, :ni], [:lit, 0]]],

  "eof!? a: b" =>
    [[:if, [:fcall, :eof!], [:vcall, :a], [:vcall, :b]]],

  "eof!?a" => [[:fcall, :eof!, [:array, [:lit, 97]]]],
  "eof!?nil:true" => [[:if, [:fcall, :eof!], [:nil], [:true]]],
  "eof??n" => [[:fcall, :eof?, [:array, [:lit, 110]]]],
  "eof??ni : 0" =>
    [[:if, [:fcall, :eof?], [:vcall, :ni], [:lit, 0]]],

  "eof??ni(&b) : 0" =>
    [[:if, [:fcall, :eof?], [:block_pass, [:vcall, :b], [:fcall,
     :ni]], [:lit, 0]]],

  "eof??ni() : 0" =>
    [[:if, [:fcall, :eof?], [:fcall, :ni], [:lit, 0]]],

  "eof??ni(*a) : 0" =>
    [[:if, [:fcall, :eof?], [:fcall, :ni, [:splat, [:vcall, :a]]
    ], [:lit, 0]]],

  "eof??ni(1) : 0" =>
    [[:if, [:fcall, :eof?], [:fcall, :ni, [:array, [:lit, 1]]], [
    :lit, 0]]],

  "eof??ni(a,b,*c){|d| e} : 0" =>
    [[:if, [:fcall, :eof?], [:iter, [:fcall, :ni, [:argscat, [
    :array, [:vcall, :a], [:vcall, :b]], [:vcall, :c]]], [
    :dasgn_curr, :d], [:vcall, :e]], [:lit, 0]]],

  "eof??ni(a,b,*c,&d) : 0" =>
    [[:if, [:fcall, :eof?], [:block_pass, [:vcall, :d], [:fcall,
     :ni, [:argscat, [:array, [:vcall, :a], [:vcall, :b]], [
    :vcall, :c]]]], [:lit, 0]]],

  "eof??ni{a} : 0" =>
    [[:if, [:fcall, :eof?], [:iter, [:fcall, :ni], nil, [:vcall,
     :a]], [:lit, 0]]],

  "eof??z.ni(a,b,*c){|d| e} : 0" => SyntaxError.new("(string):1: syntax error, unexpected ':', expecting $end\neof??z.ni(a,b,*c){|d| e} : 0\n                          ^"),
  "eof??z0.ni(a,b,*c){|d| e} : 0" =>
    [[:if, [:fcall, :eof?], [:iter, [:call, [:vcall, :z0], :ni, [
    :argscat, [:array, [:vcall, :a], [:vcall, :b]], [:vcall, :c]
    ]], [:dasgn_curr, :d], [:vcall, :e]], [:lit, 0]]],

  "eof??zz.ni(a,b,*c){|d| e} : 0" =>
    [[:if, [:fcall, :eof?], [:iter, [:call, [:vcall, :zz], :ni, [
    :argscat, [:array, [:vcall, :a], [:vcall, :b]], [:vcall, :c]
    ]], [:dasgn_curr, :d], [:vcall, :e]], [:lit, 0]]],

  "ev.length /= 2" =>
    [[:op_asgn2, [:vcall, :ev], :length=, :/, [:lit, 2]]],

  "ev.length /=2" =>
    [[:op_asgn2, [:vcall, :ev], :length=, :/, [:lit, 2]]],

  "ev.length/= 2" =>
    [[:op_asgn2, [:vcall, :ev], :length=, :/, [:lit, 2]]],

  "ev.length/=2" =>
    [[:op_asgn2, [:vcall, :ev], :length=, :/, [:lit, 2]]],

  "\
eval \\
eval(\\
#puts(\\
#!if defined?(class ENV::FRIENDLY; end)
%{$_   =
  %%%
\#{(
 (*
)=*$<
 ).map {|$_|
~%r ^#! messiness? sub( %r \#{
%.^...% (.+) }{1} nonsense, <<' >>'.
  \\1
 >>
delete( %%% <<'>>  
):'
) ) : ( (
[ [ # ] ]
  sub( %r
^ \#{   %q
(
.. %\t*\\\\s*\t
}  $)
xenon) { @_ = $1.inspect
%( $_ << \#@_ << %###\#@_
 )}\t]
][ 0 ])
  )}}
$_})
__END__
#!end
%(%(puts %(Executing Ruby code...))))\

" =>
    [[:fcall, :eval, [:array, [:fcall, :eval, [:array, [:dstr,
     "$_   =\n  %%%\n", [:evstr, [:iter, [:call, [:masgn, nil, [
    :splat], [:splat, [:gvar, :$<]]], :map], [:gasgn, :$_], [
    :if, [:call, [:lit, /^#!/min], :~], [:fcall, :sub, [:array, [
    :dregx_once, "", [:evstr, [:dot2, [:str, "^"], [:str, "(.+)"]
    ]], [:str, "{1}"], 32], [:call, [:str, "  \\1\n"], :delete, [
    :array, [:call, [:str, ""], :<<, [:array, [:str, ">>  \n):"]
    ]]]]]], [:call, [:array, [:array, [:iter, [:fcall, :sub, [
    :array, [:dregx_once, "^ ", [:evstr, [:dot2, [:str, "("], [
    :str, "*\\s*"]]], [:str, "  $)"], 18]]], nil, [:block, [
    :iasgn, :@_, [:call, [:nth_ref, 1], :inspect]], [:dstr, " $_ << ", [
    :evstr, [:ivar, :@_]], [:str, " << %###"], [:evstr, [:ivar,
     :@_]], [:str, "\n "]]]]]], :[], [:array, [:lit, 0]]]]]], [
    :str, "\n$_"]]]]]]],

  "exit" => [[:vcall, :exit]],
  "f a rescue b" =>
    [[:rescue, [:fcall, :f, [:array, [:vcall, :a]]], [:resbody,
     nil, [:vcall, :b]]]],

  "f g a rescue b" =>
    [[:rescue, [:fcall, :f, [:array, [:fcall, :g, [:array, [
    :vcall, :a]]]]], [:resbody, nil, [:vcall, :b]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "false" => [[:false]],
  "false ? P? : p8" =>
    [[:if, [:false], [:fcall, :P?], [:vcall, :p8]]],

  "false ? P?: p8" =>
    [[:if, [:false], [:fcall, :P?], [:vcall, :p8]]],

  "false ? Q : p8" =>
    [[:if, [:false], [:const, :Q], [:vcall, :p8]]],

  "false ? Q: p8" =>
    [[:if, [:false], [:const, :Q], [:vcall, :p8]]],

  "false ? p : p8" =>
    [[:if, [:false], [:vcall, :p], [:vcall, :p8]]],

  "false ? p: p8" =>
    [[:if, [:false], [:vcall, :p], [:vcall, :p8]]],

  "false ? p? : p8" =>
    [[:if, [:false], [:fcall, :p?], [:vcall, :p8]]],

  "false ? p?: p8" =>
    [[:if, [:false], [:fcall, :p?], [:vcall, :p8]]],

  "false ? q : p8" =>
    [[:if, [:false], [:vcall, :q], [:vcall, :p8]]],

  "false ? q: p8" =>
    [[:if, [:false], [:vcall, :q], [:vcall, :p8]]],

  "false ? self.<=> : p8" =>
    [[:if, [:false], [:call, [:self], :<=>], [:vcall, :p8]]],

  "false ? self.<=>: p8" =>
    [[:if, [:false], [:call, [:self], :<=>], [:vcall, :p8]]],

  "false ? self.[] : p8" =>
    [[:if, [:false], [:call, [:self], :[]], [:vcall, :p8]]],

  "false ? self.[]: p8" =>
    [[:if, [:false], [:call, [:self], :[]], [:vcall, :p8]]],

  "false and( f .. g )" =>
    [[:and, [:false], [:dot2, [:vcall, :f], [:vcall, :g]]]],

  "false and( f ... g )" =>
    [[:and, [:false], [:dot3, [:vcall, :f], [:vcall, :g]]]],

  "false and( true ? f : g )" =>
    [[:and, [:false], [:if, [:true], [:vcall, :f], [:vcall, :g]]
    ]],

  "false;a" =>
    [[:block, [:false], [:vcall, :a]],
    {:warnings=>["(string):1: warning: useless use of false in void context"]}],

  "fetch_named { {} }" =>
    [[:iter, [:fcall, :fetch_named], nil, [:hash]]],

  "filename !~ //" =>
    [[:not, [:match3, [:lit, //], [:vcall, :filename]]]],

  "filename =~ //" => [[:match3, [:lit, //], [:vcall, :filename]]],
  "first or (first,last=*rand_pos_pair)" =>
    [[:or, [:vcall, :first], [:masgn, [:array, [:lasgn, :first],
     [:lasgn, :last]], nil, [:splat, [:vcall, :rand_pos_pair]]]]
    ],

  "foo do return{a=>b} end" =>
    [[:iter, [:fcall, :foo], nil, [:return, [:hash, [:vcall, :a], [
    :vcall, :b]]]]],

  "foo do return{} end" =>
    [[:iter, [:fcall, :foo], nil, [:return, [:hash]]]],

  "foo::Bar" => [[:colon2, [:vcall, :foo], :Bar]],
  "foo=1; foo::Bar" =>
    [[:block, [:lasgn, :foo, [:lit, 1]], [:colon2, [:lvar, :foo],
     :Bar]]],

  "for (*a) in b do end" =>
    [[:for, [:vcall, :b], [:masgn, nil, [:lasgn, :a], nil]]],

  "for (a,) in b do c end" =>
    [[:for, [:vcall, :b], [:masgn, [:array, [:lasgn, :a]], nil,
     nil], [:vcall, :c]]],

  "for (a,) in b do end" =>
    [[:for, [:vcall, :b], [:masgn, [:array, [:lasgn, :a]], nil,
     nil]]],

  "for * @com_disk in a do b end" =>
    [[:for, [:vcall, :a], [:masgn, nil, [:iasgn, :@com_disk], nil], [
    :vcall, :b]]],

  "for *a in b do end" =>
    [[:for, [:vcall, :b], [:masgn, nil, [:lasgn, :a], nil]]],

  "for *a in d do e end" =>
    [[:for, [:vcall, :d], [:masgn, nil, [:lasgn, :a], nil], [
    :vcall, :e]]],

  "for - @com_disk in a do b end" => SyntaxError.new("(string):1: syntax error, unexpected tUMINUS\nfor - @com_disk in a do b end\n     ^"),
  "for a in * @com_disk do b end" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR\nfor a in * @com_disk do b end\n          ^"),
  "for a in - @com_disk do b end" =>
    [[:for, [:call, [:ivar, :@com_disk], :-@], [:lasgn, :a], [
    :vcall, :b]]],

  "for a in b do c end" =>
    [[:for, [:vcall, :b], [:lasgn, :a], [:vcall, :c]]],

  "for a in b do end" => [[:for, [:vcall, :b], [:lasgn, :a]]],
  "for a in b: c end" =>
    [[:for, [:vcall, :b], [:lasgn, :a], [:vcall, :c]]],

  "for a in b; break end" =>
    [[:for, [:vcall, :b], [:lasgn, :a], [:break]]],

  "for a in b; c end" =>
    [[:for, [:vcall, :b], [:lasgn, :a], [:vcall, :c]]],

  "for a, in b do c end" =>
    [[:for, [:vcall, :b], [:masgn, [:array, [:lasgn, :a]], nil,
     nil], [:vcall, :c]]],

  "for a,(b,*c),k,*l in d do e end" =>
    [[:for, [:vcall, :d], [:masgn, [:array, [:lasgn, :a], [:masgn, [
    :array, [:lasgn, :b]], [:lasgn, :c], nil], [:lasgn, :k]], [
    :lasgn, :l], nil], [:vcall, :e]]],

  "for a,b in c do d end" =>
    [[:for, [:vcall, :c], [:masgn, [:array, [:lasgn, :a], [
    :lasgn, :b]], nil, nil], [:vcall, :d]]],

  "for a,b in c: d end" =>
    [[:for, [:vcall, :c], [:masgn, [:array, [:lasgn, :a], [
    :lasgn, :b]], nil, nil], [:vcall, :d]]],

  "for a,b in c; d end" =>
    [[:for, [:vcall, :c], [:masgn, [:array, [:lasgn, :a], [
    :lasgn, :b]], nil, nil], [:vcall, :d]]],

  "for a.b in c do e end" =>
    [[:for, [:vcall, :c], [:attrasgn, [:vcall, :a], :b=], [
    :vcall, :e]]],

  "for a[*b] in c do end" =>
    [[:for, [:vcall, :c], [:attrasgn, [:vcall, :a], :[]=, [:splat, [
    :vcall, :b]]]]],

  "for a[b=>c,*d] in e do end" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\nfor a[b=>c,*d] in e do end\n            ^"),
  "for a[b=>c] in d do end" =>
    [[:for, [:vcall, :d], [:attrasgn, [:vcall, :a], :[]=, [:array, [
    :hash, [:vcall, :b], [:vcall, :c]]]]]],

  "for a[y,z,*b] in c do end" =>
    [[:for, [:vcall, :c], [:attrasgn, [:vcall, :a], :[]=, [
    :argscat, [:array, [:vcall, :y], [:vcall, :z]], [:vcall, :b]
    ]]]],

  "for a[z,*b] in c do end" =>
    [[:for, [:vcall, :c], [:attrasgn, [:vcall, :a], :[]=, [
    :argscat, [:array, [:vcall, :z]], [:vcall, :b]]]]],

  "for i in [] do; end" => [[:for, [:zarray], [:lasgn, :i]]],
  "\
for i in \\
[44,55,66,77,88] do p i**Math.sqrt(i) end
\n
(
for i in if
false then foob12345; else [44,55,66,77,88] end do p i**Math.sqrt(i) end
)
(
for i in if false then
foob12345; else [44,55,66,77,88] end do p i**Math.sqrt(i) end
\nc=j=0
until while j<10 do j+=1 end.nil? do p 'pppppppppp' end
for i in if false then foob12345;
else [44,55,66,77,88] end do p i**Math.sqrt(i) end
\nfor i in if false then foob12345; else
[44,55,66,77,88] end do p i**Math.sqrt(i) end
\n
for i in (c;
[44,55,66,77,88]) do p i**Math.sqrt(i) end
)
(
\nfor i in (begin
[44,55,66,77,88] end) do p i**Math.sqrt(i) end
\nfor i in if false then foob12345; else
[44,55,66,77,88] end do p i**Math.sqrt(i) end
\nfor i in (
\n[44,55,66,77,88]) do p i**Math.sqrt(i) end
\nfor i in (
[44,55,66,77,88]) do p i**Math.sqrt(i) end
\n)
\n
\ndef yy;yield end
\n" =>
    [[:block, [:for, [:array, [:lit, 44], [:lit, 55], [:lit, 66], [
    :lit, 77], [:lit, 88]], [:lasgn, :i], [:fcall, :p, [:array, [
    :call, [:lvar, :i], :**, [:array, [:call, [:const, :Math],
     :sqrt, [:array, [:lvar, :i]]]]]]]], [:for, [:if, [:false], [
    :vcall, :foob12345], [:array, [:lit, 44], [:lit, 55], [:lit,
     66], [:lit, 77], [:lit, 88]]], [:lasgn, :i], [:fcall, :p, [
    :array, [:call, [:lvar, :i], :**, [:array, [:call, [:const,
     :Math], :sqrt, [:array, [:lvar, :i]]]]]]]], [:block, [:for,
     [:if, [:false], [:vcall, :foob12345], [:array, [:lit, 44], [
    :lit, 55], [:lit, 66], [:lit, 77], [:lit, 88]]], [:lasgn, :i], [
    :fcall, :p, [:array, [:call, [:lvar, :i], :**, [:array, [
    :call, [:const, :Math], :sqrt, [:array, [:lvar, :i]]]]]]]], [
    :lasgn, :c, [:lasgn, :j, [:lit, 0]]], [:until, [:call, [
    :while, [:call, [:lvar, :j], :<, [:array, [:lit, 10]]], [
    :lasgn, :j, [:call, [:lvar, :j], :+, [:array, [:lit, 1]]]],
     true], :nil?], [:fcall, :p, [:array, [:str, "pppppppppp"]]
    ], true], [:for, [:if, [:false], [:vcall, :foob12345], [
    :array, [:lit, 44], [:lit, 55], [:lit, 66], [:lit, 77], [
    :lit, 88]]], [:lasgn, :i], [:fcall, :p, [:array, [:call, [
    :lvar, :i], :**, [:array, [:call, [:const, :Math], :sqrt, [
    :array, [:lvar, :i]]]]]]]], [:for, [:if, [:false], [:vcall,
     :foob12345], [:array, [:lit, 44], [:lit, 55], [:lit, 66], [
    :lit, 77], [:lit, 88]]], [:lasgn, :i], [:fcall, :p, [:array,
     [:call, [:lvar, :i], :**, [:array, [:call, [:const, :Math],
     :sqrt, [:array, [:lvar, :i]]]]]]]], [:for, [:block, [:lvar,
     :c], [:array, [:lit, 44], [:lit, 55], [:lit, 66], [:lit, 77], [
    :lit, 88]]], [:lasgn, :i], [:fcall, :p, [:array, [:call, [
    :lvar, :i], :**, [:array, [:call, [:const, :Math], :sqrt, [
    :array, [:lvar, :i]]]]]]]]], [:block, [:for, [:array, [:lit,
     44], [:lit, 55], [:lit, 66], [:lit, 77], [:lit, 88]], [
    :lasgn, :i], [:fcall, :p, [:array, [:call, [:lvar, :i], :**,
     [:array, [:call, [:const, :Math], :sqrt, [:array, [:lvar, :i]
    ]]]]]]], [:for, [:if, [:false], [:vcall, :foob12345], [:array, [
    :lit, 44], [:lit, 55], [:lit, 66], [:lit, 77], [:lit, 88]]],
     [:lasgn, :i], [:fcall, :p, [:array, [:call, [:lvar, :i], :**, [
    :array, [:call, [:const, :Math], :sqrt, [:array, [:lvar, :i]
    ]]]]]]], [:for, [:array, [:lit, 44], [:lit, 55], [:lit, 66],
     [:lit, 77], [:lit, 88]], [:lasgn, :i], [:fcall, :p, [:array, [
    :call, [:lvar, :i], :**, [:array, [:call, [:const, :Math],
     :sqrt, [:array, [:lvar, :i]]]]]]]], [:for, [:array, [:lit,
     44], [:lit, 55], [:lit, 66], [:lit, 77], [:lit, 88]], [
    :lasgn, :i], [:fcall, :p, [:array, [:call, [:lvar, :i], :**,
     [:array, [:call, [:const, :Math], :sqrt, [:array, [:lvar, :i]
    ]]]]]]]], [:defn, :yy, [:scope, [:block, [:args], [:yield]]]
    ]],
    {:warnings=>["(string):22: warning: useless use of a variable in void context"]}],

  "for i in begin; a; rescue b; end do gf  end" =>
    [[:for, [:begin, [:rescue, [:vcall, :a], [:resbody, [:array,
     [:vcall, :b]]]]], [:lasgn, :i], [:vcall, :gf]]],

  "f{a rescue b}" =>
    [[:iter, [:fcall, :f], nil, [:rescue, [:vcall, :a], [:resbody,
     nil, [:vcall, :b]]]]],

  "f{a=1;g{a=2}}" =>
    [[:iter, [:fcall, :f], nil, [:block, [:dasgn_curr, :a, [:lit,
     1]], [:iter, [:fcall, :g], nil, [:dasgn, :a, [:lit, 2]]]]]],

  "f{a=1;p a;g{a=2; p g}}" =>
    [[:iter, [:fcall, :f], nil, [:block, [:dasgn_curr, :a, [:lit,
     1]], [:fcall, :p, [:array, [:dvar, :a]]], [:iter, [:fcall,
     :g], nil, [:block, [:dasgn, :a, [:lit, 2]], [:fcall, :p, [
    :array, [:vcall, :g]]]]]]]],

  "f{|a,*|d}" =>
    [[:iter, [:fcall, :f], [:masgn, [:array, [:dasgn_curr, :a]],
     [:splat], nil], [:vcall, :d]]],

  "f{|a,b,*|d}" =>
    [[:iter, [:fcall, :f], [:masgn, [:array, [:dasgn_curr, :a], [
    :dasgn_curr, :b]], [:splat], nil], [:vcall, :d]]],

  "f{|a,b,|d}" =>
    [[:iter, [:fcall, :f], [:masgn, [:array, [:dasgn_curr, :a], [
    :dasgn_curr, :b]], nil, nil], [:vcall, :d]]],

  "f{|a,|d}" =>
    [[:iter, [:fcall, :f], [:masgn, [:array, [:dasgn_curr, :a]],
     nil, nil], [:vcall, :d]]],

  "f{|aa,(a,)|d}" =>
    [[:iter, [:fcall, :f], [:masgn, [:array, [:dasgn_curr, :aa],
     [:masgn, [:array, [:dasgn_curr, :a]], nil, nil]], nil, nil], [
    :vcall, :d]]],

  "f{|aa,(a,*)|d}" =>
    [[:iter, [:fcall, :f], [:masgn, [:array, [:dasgn_curr, :aa],
     [:masgn, [:array, [:dasgn_curr, :a]], [:splat], nil]], nil,
     nil], [:vcall, :d]]],

  "f{|aa,(a,b,)|d}" =>
    [[:iter, [:fcall, :f], [:masgn, [:array, [:dasgn_curr, :aa],
     [:masgn, [:array, [:dasgn_curr, :a], [:dasgn_curr, :b]], nil,
     nil]], nil, nil], [:vcall, :d]]],

  "f{|aa,(a,b,*)|d}" =>
    [[:iter, [:fcall, :f], [:masgn, [:array, [:dasgn_curr, :aa],
     [:masgn, [:array, [:dasgn_curr, :a], [:dasgn_curr, :b]], [
    :splat], nil]], nil, nil], [:vcall, :d]]],

  "h a=>p (1).metho" =>
    [[:fcall, :h, [:array, [:hash, [:vcall, :a], [:call, [:fcall,
     :p, [:array, [:lit, 1]]], :metho]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "h a=>p (1).metho,2" => SyntaxError.new("(string):1: syntax error, unexpected $end, expecting tASSOC\nh a=>p (1).metho,2\n                  ^"),
  "h(a=>p (1).metho)" =>
    [[:fcall, :h, [:array, [:hash, [:vcall, :a], [:call, [:fcall,
     :p, [:array, [:lit, 1]]], :metho]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "h(a=>p (1).metho,2)" => SyntaxError.new("(string):1: syntax error, unexpected ')', expecting tASSOC\nh(a=>p (1).metho,2)\n                   ^"),
  "have_header(*curses=%w\"ncurses.h\")" =>
    [[:fcall, :have_header, [:splat, [:lasgn, :curses, [:array, [
    :str, "ncurses.h"]]]]]],

  "i=99" => [[:lasgn, :i, [:lit, 99]]],
  "i? &R" =>
    [[:block_pass, [:const, :R], [:fcall, :i?]],
    {:warnings=>["(string):1: warning: `&' interpreted as argument prefix"]}],

  "i? *R" =>
    [[:fcall, :i?, [:splat, [:const, :R]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "i? +R" =>
    [[:fcall, :i?, [:array, [:call, [:const, :R], :+@]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "i? -R" =>
    [[:fcall, :i?, [:array, [:call, [:const, :R], :-@]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "i? ::R" => [[:fcall, :i?, [:array, [:colon3, :R]]]],
  "i?::R" => [[:colon2, [:fcall, :i?], :R]],
  "if (true) :foo end" => [[:if, [:true], [:vcall, :foo], nil]],
  "if * @com_disk;end" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR\nif * @com_disk;end\n    ^"),
  "if - @com_disk;end" =>
    [[:if, [:call, [:ivar, :@com_disk], :-@], nil, nil]],

  "if a then b end" => [[:if, [:vcall, :a], [:vcall, :b], nil]],
  "if a+0 then b end" =>
    [[:if, [:call, [:vcall, :a], :+, [:array, [:lit, 0]]], [
    :vcall, :b], nil]],

  "if a+0: b end" =>
    [[:if, [:call, [:vcall, :a], :+, [:array, [:lit, 0]]], [
    :vcall, :b], nil]],

  "if a+0; b else c end" =>
    [[:if, [:call, [:vcall, :a], :+, [:array, [:lit, 0]]], [
    :vcall, :b], [:vcall, :c]]],

  "if a+0; b elsif c; d else e end" =>
    [[:if, [:call, [:vcall, :a], :+, [:array, [:lit, 0]]], [
    :vcall, :b], [:if, [:vcall, :c], [:vcall, :d], [:vcall, :e]]
    ]],

  "if a+0; b elsif c; d end" =>
    [[:if, [:call, [:vcall, :a], :+, [:array, [:lit, 0]]], [
    :vcall, :b], [:if, [:vcall, :c], [:vcall, :d], nil]]],

  "if a+0; b end" =>
    [[:if, [:call, [:vcall, :a], :+, [:array, [:lit, 0]]], [
    :vcall, :b], nil]],

  "if a+0; else c end" =>
    [[:if, [:call, [:vcall, :a], :+, [:array, [:lit, 0]]], nil, [
    :vcall, :c]]],

  "if a+0; elsif c; d end" =>
    [[:if, [:call, [:vcall, :a], :+, [:array, [:lit, 0]]], nil, [
    :if, [:vcall, :c], [:vcall, :d], nil]]],

  "if a: b end" => [[:if, [:vcall, :a], [:vcall, :b], nil]],
  "if a; b else c end" =>
    [[:if, [:vcall, :a], [:vcall, :b], [:vcall, :c]]],

  "if a; b elsif c; d else e end" =>
    [[:if, [:vcall, :a], [:vcall, :b], [:if, [:vcall, :c], [
    :vcall, :d], [:vcall, :e]]]],

  "if a; b elsif c; d end" =>
    [[:if, [:vcall, :a], [:vcall, :b], [:if, [:vcall, :c], [
    :vcall, :d], nil]]],

  "if a; b end" => [[:if, [:vcall, :a], [:vcall, :b], nil]],
  "if a; b; elsif * @com_disk; c end" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR\nif a; b; elsif * @com_disk; c end\n                ^"),
  "if a; b; elsif - @com_disk; c end" =>
    [[:if, [:vcall, :a], [:vcall, :b], [:if, [:call, [
    :ivar, :@com_disk], :-@], [:vcall, :c], nil]]],

  "if a; b; elsif c; else d end" =>
    [[:if, [:vcall, :a], [:vcall, :b], [:if, [:vcall, :c], nil, [
    :vcall, :d]]]],

  "if a; else * @com_disk;end" => SyntaxError.new("(string):1: syntax error, unexpected ';', expecting '='\nif a; else * @com_disk;end\n                       ^"),
  "if a; else - @com_disk;end" =>
    [[:if, [:vcall, :a], nil, [:call, [:ivar, :@com_disk], :-@]]
    ],

  "if a; else c end" => [[:if, [:vcall, :a], nil, [:vcall, :c]]],
  "if a; elsif c; d else e end" =>
    [[:if, [:vcall, :a], nil, [:if, [:vcall, :c], [:vcall, :d], [
    :vcall, :e]]]],

  "if a; elsif c; d end" =>
    [[:if, [:vcall, :a], nil, [:if, [:vcall, :c], [:vcall, :d],
     nil]]],

  "if begin; a; rescue b; end then c end" =>
    [[:if, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], [:vcall, :c], nil]],

  "if defined? b : c end" =>
    [[:if, [:defined, [:vcall, :b]], [:vcall, :c], nil]],

  "\
if false
  p+1
  p&1
  p|1
  p^1
  p%1
  p*1
\n  p A:B
end\

" =>
    [[:if, [:false], [:block, [:call, [:vcall, :p], :+, [:array,
     [:lit, 1]]], [:call, [:vcall, :p], :&, [:array, [:lit, 1]]
    ], [:call, [:vcall, :p], :|, [:array, [:lit, 1]]], [:call, [
    :vcall, :p], :^, [:array, [:lit, 1]]], [:call, [:vcall, :p],
     :%, [:array, [:lit, 1]]], [:call, [:vcall, :p], :*, [:array, [
    :lit, 1]]], [:fcall, :p, [:array, [:fcall, :A, [:array, [
    :lit, :B]]]]]], nil],
    {:warnings=>["(string):9: warning: parenthesize argument(s) for future version", "(string):2: warning: useless use of + in void context", "(string):3: warning: useless use of & in void context", "(string):4: warning: useless use of | in void context", "(string):5: warning: useless use of ^ in void context", "(string):6: warning: useless use of % in void context", "(string):7: warning: useless use of * in void context"]}],

  "if m; n; elsif begin; a; rescue b; end then o end" =>
    [[:if, [:vcall, :m], [:vcall, :n], [:if, [:rescue, [:vcall,
     :a], [:resbody, [:array, [:vcall, :b]]]], [:vcall, :o], nil]
    ]],

  "if not a; b end" => [[:if, [:vcall, :a], nil, [:vcall, :b]]],
  "if not b : c end" => [[:if, [:vcall, :b], nil, [:vcall, :c]]],
  "if o then begin end else l end" =>
    [[:if, [:vcall, :o], [:nil], [:vcall, :l]]],

  "if p (1).m; end" =>
    [[:if, [:call, [:fcall, :p, [:array, [:lit, 1]]], :m], nil,
     nil],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "if p then p end" => [[:if, [:vcall, :p], [:vcall, :p], nil]],
  "\
if true
  puts 1234
end
\n
puts \"1234\" if true\

" =>
    [[:block, [:if, [:true], [:fcall, :puts, [:array, [:lit, 1234]
    ]], nil], [:if, [:true], [:fcall, :puts, [:array, [:str, "1234"]
    ]], nil]]],

  "if v; else return ([vUew]).e  end" =>
    [[:if, [:vcall, :v], nil, [:return, [:call, [:array, [:vcall,
     :vUew]], :e]]]],

  "if v; else return([vUew]).e  end" =>
    [[:if, [:vcall, :v], nil, [:return, [:call, [:array, [:vcall,
     :vUew]], :e]]]],

  "\
j=9;def i(n) [n ?\"d\" : \"e\" , n] end
\np(i ?\")
p(j ?\"d\" : \"e\")
\ndef g(x=nil) x end
def gg(x=nil) g x end
p(gg :x)
p(g :y)
g=9
p(gg :z)
# g :w #error
\n
begin
a
rescue
b;c
end
\n" =>
    [[:block, [:lasgn, :j, [:lit, 9]], [:defn, :i, [:scope, [
    :block, [:args, :n], [:array, [:if, [:lvar, :n], [:str, "d"], [
    :str, "e"]], [:lvar, :n]]]]], [:fcall, :p, [:array, [:fcall,
     :i, [:array, [:lit, 34]]]]], [:fcall, :p, [:array, [:if, [
    :lvar, :j], [:str, "d"], [:str, "e"]]]], [:defn, :g, [:scope, [
    :block, [:args, :x, [:block, [:lasgn, :x, [:nil]]]], [:lvar,
     :x]]]], [:defn, :gg, [:scope, [:block, [:args, :x, [:block,
     [:lasgn, :x, [:nil]]]], [:fcall, :g, [:array, [:lvar, :x]]]
    ]]], [:fcall, :p, [:array, [:fcall, :gg, [:array, [:lit, :x]
    ]]]], [:fcall, :p, [:array, [:fcall, :g, [:array, [:lit, :y]
    ]]]], [:lasgn, :g, [:lit, 9]], [:fcall, :p, [:array, [:fcall,
     :gg, [:array, [:lit, :z]]]]], [:rescue, [:vcall, :a], [
    :resbody, nil, [:block, [:vcall, :b], [:vcall, :c]]]]],
    {:warnings=>["(string):3: warning: parenthesize argument(s) for future version", "(string):8: warning: parenthesize argument(s) for future version", "(string):9: warning: parenthesize argument(s) for future version", "(string):11: warning: parenthesize argument(s) for future version"]}],

  "k ? a0 = a rescue b : d" =>
    [[:if, [:vcall, :k], [:lasgn, :a0, [:rescue, [:vcall, :a], [
    :resbody, nil, [:vcall, :b]]]], [:vcall, :d]]],

  "k=1; break - k" =>
    [[:block, [:lasgn, :k, [:lit, 1]], [:break, [:call, [:lvar,
     :k], :-@]]]],

  "k=1; next - k" =>
    [[:block, [:lasgn, :k, [:lit, 1]], [:next, [:call, [:lvar,
     :k], :-@]]]],

  "k=1; return * k" =>
    [[:block, [:lasgn, :k, [:lit, 1]], [:return, [:svalue, [
    :splat, [:lvar, :k]]]]]],

  "k=1; return + k" =>
    [[:block, [:lasgn, :k, [:lit, 1]], [:return, [:call, [:lvar,
     :k], :+@]]]],

  "k=1; return - k" =>
    [[:block, [:lasgn, :k, [:lit, 1]], [:return, [:call, [:lvar,
     :k], :-@]]]],

  "k=1; return -k" =>
    [[:block, [:lasgn, :k, [:lit, 1]], [:return, [:call, [:lvar,
     :k], :-@]]]],

  "k=1; return- k" =>
    [[:block, [:lasgn, :k, [:lit, 1]], [:return, [:call, [:lvar,
     :k], :-@]]]],

  "k=1; return-k" =>
    [[:block, [:lasgn, :k, [:lit, 1]], [:return, [:call, [:lvar,
     :k], :-@]]]],

  "k=Hash;class CornedBeef<k; end" =>
    [[:block, [:lasgn, :k, [:const, :Hash]], [:class, :CornedBeef, [
    :lvar, :k], [:scope]]]],

  "k=z=c,d" =>
    [[:lasgn, :k, [:svalue, [:array, [:lasgn, :z, [:vcall, :c]],
     [:vcall, :d]]]]],

  "k=z=c,d rescue b" =>
    [[:rescue, [:lasgn, :k, [:svalue, [:array, [:lasgn, :z, [
    :vcall, :c]], [:vcall, :d]]]], [:resbody, nil, [:vcall, :b]]
    ]],

  "k{b,(i,j,c.f,g[h],*d),*e=z}" =>
    [[:iter, [:fcall, :k], nil, [:masgn, [:array, [:dasgn_curr,
     :b], [:masgn, [:array, [:dasgn_curr, :i], [:dasgn_curr, :j], [
    :attrasgn, [:vcall, :c], :f=], [:attrasgn, [:vcall, :g], :[]=, [
    :array, [:vcall, :h]]]], [:dasgn_curr, :d], nil]], [
    :dasgn_curr, :e], [:to_ary, [:vcall, :z]]]]],

  "\
l rescue
r" =>
    [[:rescue, [:vcall, :l], [:resbody, nil, [:vcall, :r]]]],

  "\
label='label';tab=[1,2,3]
      p <<S
\#{label} = arr = Array.new(\#{tab.size}, nil)
str = a = i = nil
idx = 0
clist.each do |str|
  str.split(',', -1).each do |i|
    arr[idx] = i.to_i unless i.empty?
    idx += 1
  end
end
S\

" =>
    [[:block, [:lasgn, :label, [:str, "label"]], [:lasgn, :tab, [
    :array, [:lit, 1], [:lit, 2], [:lit, 3]]], [:fcall, :p, [
    :array, [:dstr, "", [:evstr, [:lvar, :label]], [
    :str, " = arr = Array.new("], [:evstr, [:call, [:lvar, :tab],
     :size]], [
    :str, ", nil)\nstr = a = i = nil\nidx = 0\nclist.each do |str|\n  str.split(',', -1).each do |i|\n    arr[idx] = i.to_i unless i.empty?\n    idx += 1\n  end\nend\n"]
    ]]]]],

  "list()" => [[:fcall, :list]],
  "list(3)" => [[:fcall, :list, [:array, [:lit, 3]]]],
  "list(3,4)" => [[:fcall, :list, [:array, [:lit, 3], [:lit, 4]]]],
  "loop do break end" => [[:iter, [:fcall, :loop], nil, [:break]]],
  "loop { break 1,2 }" =>
    [[:iter, [:fcall, :loop], nil, [:break, [:array, [:lit, 1], [
    :lit, 2]]]]],

  "loop { break 1,2,*[3,4] }" =>
    [[:iter, [:fcall, :loop], nil, [:break, [:argscat, [:array, [
    :lit, 1], [:lit, 2]], [:array, [:lit, 3], [:lit, 4]]]]]],

  "loop { break 1}" =>
    [[:iter, [:fcall, :loop], nil, [:break, [:lit, 1]]]],

  "loop { break }" => [[:iter, [:fcall, :loop], nil, [:break]]],
  "loop { break() }" =>
    [[:iter, [:fcall, :loop], nil, [:break, [:nil]]]],

  "loop { break(1)}" =>
    [[:iter, [:fcall, :loop], nil, [:break, [:lit, 1]]]],

  "loop{break{a=>b}}" =>
    [[:iter, [:fcall, :loop], nil, [:break, [:hash, [:vcall, :a], [
    :vcall, :b]]]]],

  "loop{break{}.size}" =>
    [[:iter, [:fcall, :loop], nil, [:break, [:call, [:hash], :size]
    ]]],

  "loop{break{}}" =>
    [[:iter, [:fcall, :loop], nil, [:break, [:hash]]]],

  "m % begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :%, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m %= begin; a; rescue b; end" =>
    [[:lasgn, :m, [:call, [:lvar, :m], :%, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "m & begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :&, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m && !begin; a; rescue b; end" =>
    [[:and, [:vcall, :m], [:not, [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]]],

  "m && (begin; a; rescue b; end)" =>
    [[:and, [:vcall, :m], [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]],

  "m && begin; a; rescue b; end" =>
    [[:and, [:vcall, :m], [:begin, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m &&= begin; a; rescue b; end" =>
    [[:op_asgn_and, [:lvar, :m], [:lasgn, :m, [:rescue, [:vcall,
     :a], [:resbody, [:array, [:vcall, :b]]]]]]],

  "m &= begin; a; rescue b; end" =>
    [[:lasgn, :m, [:call, [:lvar, :m], :&, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "m &begin; a; rescue b; end" =>
    [[:block_pass, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]], [:fcall, :m]],
    {:warnings=>["(string):1: warning: `&' interpreted as argument prefix"]}],

  "m * begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :*, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m ** begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :**, [:array, [:rescue, [:vcall, :a],
     [:resbody, [:array, [:vcall, :b]]]]]]],

  "m **= begin; a; rescue b; end" =>
    [[:lasgn, :m, [:call, [:lvar, :m], :**, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "m *= begin; a; rescue b; end" =>
    [[:lasgn, :m, [:call, [:lvar, :m], :*, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "m *begin; a; rescue b; end" =>
    [[:fcall, :m, [:splat, [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "m + begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :+, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m += begin; a; rescue b; end" =>
    [[:lasgn, :m, [:call, [:lvar, :m], :+, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "m - begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :-, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m -= begin; a; rescue b; end" =>
    [[:lasgn, :m, [:call, [:lvar, :m], :-, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "m / begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :/, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m /= begin; a; rescue b; end" =>
    [[:lasgn, :m, [:call, [:lvar, :m], :/, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "m ; begin; a; rescue b; end" =>
    [[:block, [:vcall, :m], [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]],

  "m < begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :<, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m << begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :<<, [:array, [:rescue, [:vcall, :a],
     [:resbody, [:array, [:vcall, :b]]]]]]],

  "m <<= begin; a; rescue b; end" =>
    [[:lasgn, :m, [:call, [:lvar, :m], :<<, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "m <= begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :<=, [:array, [:rescue, [:vcall, :a],
     [:resbody, [:array, [:vcall, :b]]]]]]],

  "m <=> begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :<=>, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m = begin; a; rescue b; end" =>
    [[:lasgn, :m, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]]]],

  "m == begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :==, [:array, [:rescue, [:vcall, :a],
     [:resbody, [:array, [:vcall, :b]]]]]]],

  "m === begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :===, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m > begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :>, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m >= begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :>=, [:array, [:rescue, [:vcall, :a],
     [:resbody, [:array, [:vcall, :b]]]]]]],

  "m >> begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :>>, [:array, [:rescue, [:vcall, :a],
     [:resbody, [:array, [:vcall, :b]]]]]]],

  "m >>= begin; a; rescue b; end" =>
    [[:lasgn, :m, [:call, [:lvar, :m], :>>, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "m ? !begin; a; rescue b; end : nil" =>
    [[:if, [:vcall, :m], [:not, [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]], [:nil]]],

  "m ? (begin; a; rescue b; end) : nil" =>
    [[:if, [:vcall, :m], [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]], [:nil]]],

  "m ? begin; a; rescue b; end : nil" =>
    [[:if, [:vcall, :m], [:begin, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]], [:nil]]],

  "m ? n : (b)" =>
    [[:if, [:vcall, :m], [:vcall, :n], [:vcall, :b]]],

  "m ? n : (begin; end)" =>
    [[:if, [:vcall, :m], [:vcall, :n], [:nil]]],

  "m ? nil : !begin; a; rescue b; end" =>
    [[:if, [:vcall, :m], [:nil], [:not, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m ? nil : (begin; a; rescue b; end)" =>
    [[:if, [:vcall, :m], [:nil], [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]],

  "m ? nil : begin; a; rescue b; end" =>
    [[:if, [:vcall, :m], [:nil], [:begin, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m ^ begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :^, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m ^= begin; a; rescue b; end" =>
    [[:lasgn, :m, [:call, [:lvar, :m], :^, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "m and !begin; a; rescue b; end" =>
    [[:and, [:vcall, :m], [:not, [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]]],

  "m and (begin; a; rescue b; end)" =>
    [[:and, [:vcall, :m], [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]],

  "m and begin; a; rescue b; end" =>
    [[:and, [:vcall, :m], [:begin, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m if begin; a; rescue b; end" =>
    [[:if, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], [:vcall, :m], nil]],

  "m or !begin; a; rescue b; end" =>
    [[:or, [:vcall, :m], [:not, [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]]],

  "m or (begin; a; rescue b; end)" =>
    [[:or, [:vcall, :m], [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]],

  "m or begin; a; rescue b; end" =>
    [[:or, [:vcall, :m], [:begin, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m rescue begin; a; rescue b; end" =>
    [[:rescue, [:vcall, :m], [:resbody, nil, [:rescue, [:vcall,
     :a], [:resbody, [:array, [:vcall, :b]]]]]]],

  "m unless begin; a; rescue b; end" =>
    [[:if, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], nil, [:vcall, :m]]],

  "m until begin; a; rescue b; end" =>
    [[:until, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]], [:vcall, :m], true]],

  "m while begin; a; rescue b; end" =>
    [[:while, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]], [:vcall, :m], true]],

  "m | begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :|, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m |= begin; a; rescue b; end" =>
    [[:lasgn, :m, [:call, [:lvar, :m], :|, [:array, [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "m || !begin; a; rescue b; end" =>
    [[:or, [:vcall, :m], [:not, [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]]],

  "m || (begin; a; rescue b; end)" =>
    [[:or, [:vcall, :m], [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]],

  "m || begin; a; rescue b; end" =>
    [[:or, [:vcall, :m], [:begin, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m ||= begin; a; rescue b; end" =>
    [[:op_asgn_or, [:lvar, :m], [:lasgn, :m, [:rescue, [:vcall,
     :a], [:resbody, [:array, [:vcall, :b]]]]]]],

  "m(&begin; a; rescue b; end)" =>
    [[:block_pass, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]], [:fcall, :m]]],

  "m(*begin; a; rescue b; end)" =>
    [[:fcall, :m, [:splat, [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]]],

  "m+begin; a; rescue b; end" =>
    [[:call, [:vcall, :m], :+, [:array, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]],

  "m,(a[*b],)=c" =>
    [[:masgn, [:array, [:lasgn, :m], [:masgn, [:array, [:attrasgn, [
    :vcall, :a], :[]=, [:splat, [:vcall, :b]]]], nil, nil]], nil, [
    :to_ary, [:vcall, :c]]]],

  "m,(a[b=>c,*d],)=e" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\nm,(a[b=>c,*d],)=e\n           ^"),
  "m,(a[b=>c],)=d" =>
    [[:masgn, [:array, [:lasgn, :m], [:masgn, [:array, [:attrasgn, [
    :vcall, :a], :[]=, [:array, [:hash, [:vcall, :b], [:vcall, :c]
    ]]]], nil, nil]], nil, [:to_ary, [:vcall, :d]]]],

  "m,(a[y,z,*b],)=c" =>
    [[:masgn, [:array, [:lasgn, :m], [:masgn, [:array, [:attrasgn, [
    :vcall, :a], :[]=, [:argscat, [:array, [:vcall, :y], [:vcall,
     :z]], [:vcall, :b]]]], nil, nil]], nil, [:to_ary, [:vcall,
     :c]]]],

  "m,(a[z,*b],)=c" =>
    [[:masgn, [:array, [:lasgn, :m], [:masgn, [:array, [:attrasgn, [
    :vcall, :a], :[]=, [:argscat, [:array, [:vcall, :z]], [
    :vcall, :b]]]], nil, nil]], nil, [:to_ary, [:vcall, :c]]]],

  "m,*a[*b]=c" =>
    [[:masgn, [:array, [:lasgn, :m]], [:attrasgn, [:vcall, :a],
     :[]=, [:splat, [:vcall, :b]]], [:to_ary, [:vcall, :c]]]],

  "m,*a[b=>c,*d]=e" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\nm,*a[b=>c,*d]=e\n           ^"),
  "m,*a[b=>c]=d" =>
    [[:masgn, [:array, [:lasgn, :m]], [:attrasgn, [:vcall, :a],
     :[]=, [:array, [:hash, [:vcall, :b], [:vcall, :c]]]], [
    :to_ary, [:vcall, :d]]]],

  "m,*a[y,z,*b]=c" =>
    [[:masgn, [:array, [:lasgn, :m]], [:attrasgn, [:vcall, :a],
     :[]=, [:argscat, [:array, [:vcall, :y], [:vcall, :z]], [
    :vcall, :b]]], [:to_ary, [:vcall, :c]]]],

  "m,*a[z,*b]=c" =>
    [[:masgn, [:array, [:lasgn, :m]], [:attrasgn, [:vcall, :a],
     :[]=, [:argscat, [:array, [:vcall, :z]], [:vcall, :b]]], [
    :to_ary, [:vcall, :c]]]],

  "m.alias * com_disk" =>
    [[:call, [:call, [:vcall, :m], :alias], :*, [:array, [:vcall,
     :com_disk]]]],

  "m.alias - com_disk" =>
    [[:call, [:call, [:vcall, :m], :alias], :-, [:array, [:vcall,
     :com_disk]]]],

  "m.begin * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :begin], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.begin - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :begin], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.break * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :break], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.break - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :break], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.catch * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :catch], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.catch - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :catch], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.continue * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :continue], :*, [:array, [
    :ivar, :@com_disk]]]],

  "m.continue - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :continue], :-, [:array, [
    :ivar, :@com_disk]]]],

  "m.defined? * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :defined?], :*, [:array, [
    :ivar, :@com_disk]]]],

  "m.defined? - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :defined?], :-, [:array, [
    :ivar, :@com_disk]]]],

  "m.do * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :do], :*, [:array, [
    :ivar, :@com_disk]]]],

  "m.do - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :do], :-, [:array, [
    :ivar, :@com_disk]]]],

  "m.else * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :else], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.else - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :else], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.elsif * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :elsif], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.elsif - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :elsif], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.end * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :end], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.end - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :end], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.ensure * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :ensure], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.ensure - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :ensure], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.for * @com_disk " =>
    [[:call, [:call, [:vcall, :m], :for], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.for - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :for], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.if * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :if], :*, [:array, [
    :ivar, :@com_disk]]]],

  "m.if - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :if], :-, [:array, [
    :ivar, :@com_disk]]]],

  "m.in * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :in], :*, [:array, [
    :ivar, :@com_disk]]]],

  "m.in - @com_disk " =>
    [[:call, [:call, [:vcall, :m], :in], :-, [:array, [
    :ivar, :@com_disk]]]],

  "m.next * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :next], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.next - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :next], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.raise * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :raise], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.raise - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :raise], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.rescue * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :rescue], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.rescue *a=r" =>
    [[:call, [:vcall, :m], :rescue, [:splat, [:lasgn, :a, [
    :vcall, :r]]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "m.rescue - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :rescue], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.rescue a=r,b" =>
    [[:call, [:vcall, :m], :rescue, [:array, [:lasgn, :a, [
    :vcall, :r]], [:vcall, :b]]]],

  "m.return * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :return], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.throw * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :throw], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.throw - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :throw], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.undef * com_disk" =>
    [[:call, [:call, [:vcall, :m], :undef], :*, [:array, [:vcall,
     :com_disk]]]],

  "m.undef - com_disk" =>
    [[:call, [:call, [:vcall, :m], :undef], :-, [:array, [:vcall,
     :com_disk]]]],

  "m.unless * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :unless], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.unless - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :unless], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.until * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :until], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.until - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :until], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.when *a=r" =>
    [[:call, [:vcall, :m], :when, [:splat, [:lasgn, :a, [:vcall,
     :r]]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "m.when a=r,b" =>
    [[:call, [:vcall, :m], :when, [:array, [:lasgn, :a, [:vcall,
     :r]], [:vcall, :b]]]],

  "m.while * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :while], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.while - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :while], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m.yield * @com_disk" =>
    [[:call, [:call, [:vcall, :m], :yield], :*, [:array, [:ivar,
     :@com_disk]]]],

  "m.yield *a=r" =>
    [[:call, [:vcall, :m], :yield, [:splat, [:lasgn, :a, [:vcall,
     :r]]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "m.yield - @com_disk" =>
    [[:call, [:call, [:vcall, :m], :yield], :-, [:array, [:ivar,
     :@com_disk]]]],

  "m=*begin; a; rescue b; end" =>
    [[:lasgn, :m, [:svalue, [:splat, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]]]]]],

  "module ::A::B::Special::Extend C end" =>
    [[:module, [:colon2, [:colon2, [:colon2, [:colon3, :A], :B],
     :Special], :Extend], [:scope, [:const, :C]]]],

  "module ::BUG::ONE extend OptiFlag::Flagset; end" =>
    [[:module, [:colon2, [:colon3, :BUG], :ONE], [:scope, [
    :fcall, :extend, [:array, [:colon2, [:const, :OptiFlag], :Flagset]
    ]]]]],

  "module ::BUG_ONE extend OptiFlag::Flagset; end" =>
    [[:module, [:colon3, :BUG_ONE], [:scope, [:fcall, :extend, [
    :array, [:colon2, [:const, :OptiFlag], :Flagset]]]]]],

  "module A 55 end" => [[:module, :A, [:scope, [:lit, 55]]]],
  "module A::B :: Special::Extend C end" =>
    [[:module, [:colon2, [:const, :A], :B], [:scope, [:call, [
    :colon3, :Special], :Extend, [:array, [:const, :C]]]]]],

  "module A::B ::Special::Extend C end" =>
    [[:module, [:colon2, [:const, :A], :B], [:scope, [:call, [
    :colon3, :Special], :Extend, [:array, [:const, :C]]]]]],

  "module A::B:: Special::Extend C end" =>
    [[:module, [:colon2, [:colon2, [:colon2, [:const, :A], :B],
     :Special], :Extend], [:scope, [:const, :C]]]],

  "module A::B::Special::Extend C end" =>
    [[:module, [:colon2, [:colon2, [:colon2, [:const, :A], :B],
     :Special], :Extend], [:scope, [:const, :C]]]],

  "module A::B; end" =>
    [[:module, [:colon2, [:const, :A], :B], [:scope]]],

  "\
module A::
=begin =end
=end
 B; end" =>
    [[:module, [:colon2, [:const, :A], :B], [:scope]]],

  "module A; B='' end" =>
    [[:module, :A, [:scope, [:cdecl, :B, [:str, ""]]]]],

  "module A; b end" => [[:module, :A, [:scope, [:vcall, :b]]]],
  "module A; b; rescue C=>d; e; else g; ensure f; end" =>
    [[:module, :A, [:scope, [:ensure, [:rescue, [:vcall, :b], [
    :resbody, [:array, [:const, :C]], [:block, [:lasgn, :d, [
    :gvar, :$!]], [:vcall, :e]]], [:vcall, :g]], [:vcall, :f]]]]
    ],

  "module A; b;c end" =>
    [[:module, :A, [:scope, [:block, [:vcall, :b], [:vcall, :c]]
    ]]],

  "module A; end" => [[:module, :A, [:scope]]],
  "\
module A
=begin =end
=end
 ::B; end" =>
    [[:module, :A, [:scope, [:colon3, :B]]]],

  "module Array ([Array]).first::E include M end" =>
    [[:module, [:colon2, [:call, [:fcall, :Array, [:array, [
    :array, [:const, :Array]]]], :first], :E], [:scope, [:fcall,
     :include, [:array, [:const, :M]]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "module Array ([Array]).first::E include M; end" =>
    [[:module, [:colon2, [:call, [:fcall, :Array, [:array, [
    :array, [:const, :Array]]]], :first], :E], [:scope, [:fcall,
     :include, [:array, [:const, :M]]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "module Array ([Array]).first::E; end" =>
    [[:module, [:colon2, [:call, [:fcall, :Array, [:array, [
    :array, [:const, :Array]]]], :first], :E], [:scope]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "module Array([Array]).first::E include M end" =>
    [[:module, [:colon2, [:call, [:fcall, :Array, [:array, [
    :array, [:const, :Array]]]], :first], :E], [:scope, [:fcall,
     :include, [:array, [:const, :M]]]]]],

  "module Array([Array]).first::E include M; end" =>
    [[:module, [:colon2, [:call, [:fcall, :Array, [:array, [
    :array, [:const, :Array]]]], :first], :E], [:scope, [:fcall,
     :include, [:array, [:const, :M]]]]]],

  "module Array([Array]).first::E; end" =>
    [[:module, [:colon2, [:call, [:fcall, :Array, [:array, [
    :array, [:const, :Array]]]], :first], :E], [:scope]]],

  "module BUG::ONE extend OptiFlag::Flagset; end" =>
    [[:module, [:colon2, [:const, :BUG], :ONE], [:scope, [:fcall,
     :extend, [:array, [:colon2, [:const, :OptiFlag], :Flagset]]
    ]]]],

  "module BUG_ONE extend OptiFlag::Flagset; end" =>
    [[:module, :BUG_ONE, [:scope, [:fcall, :extend, [:array, [
    :colon2, [:const, :OptiFlag], :Flagset]]]]]],

  "module BUG_ONE; extend OptiFlag::Flagset; end" =>
    [[:module, :BUG_ONE, [:scope, [:fcall, :extend, [:array, [
    :colon2, [:const, :OptiFlag], :Flagset]]]]]],

  "module BUNE end" => [[:module, :BUNE, [:scope]]],
  "module BUNE extend OptiFgset end" =>
    [[:module, :BUNE, [:scope, [:fcall, :extend, [:array, [
    :const, :OptiFgset]]]]]],

  "module BUNE extend end" =>
    [[:module, :BUNE, [:scope, [:vcall, :extend]]]],

  "\
module Defined_p_syntax_tests
  def self.defined?(foo) :baz end  #should be methname
  def defined?(foo) :bar end  #should be methname
  def ameth
    p(defined? 44)  #should be keyword
    p(self.defined? 44) #should be methname
  end
end\

" =>
    [[:module, :Defined_p_syntax_tests, [:scope, [:block, [:defs, [
    :self], :defined?, [:scope, [:block, [:args, :foo], [:lit,
     :baz]]]], [:defn, :defined?, [:scope, [:block, [:args, :foo], [
    :lit, :bar]]]], [:defn, :ameth, [:scope, [:block, [:args], [
    :fcall, :p, [:array, [:defined, [:lit, 44]]]], [:fcall, :p, [
    :array, [:call, [:self], :defined?, [:array, [:lit, 44]]]]]]
    ]]]]],
    {:warnings=>["(string):6: warning: parenthesize argument(s) for future version"]}],

  "\
module SR
def SelfReferencing
#old name alias
end
def SelfReferencing #old name alias
end
def SelfReferencing#old name alias
end
def SelfReferencing;end#old name alias
end\

" =>
    [[:module, :SR, [:scope, [:block, [:defn, :SelfReferencing, [
    :scope, [:block, [:args], [:nil]]]], [:defn, :SelfReferencing, [
    :scope, [:block, [:args], [:nil]]]], [:defn, :SelfReferencing, [
    :scope, [:block, [:args], [:nil]]]], [:defn, :SelfReferencing, [
    :scope, [:block, [:args], [:nil]]]]]]]],

  "\
module TYDFG  
  p a ;
end" =>
    [[:module, :TYDFG, [:scope, [:fcall, :p, [:array, [:vcall, :a]
    ]]]]],

  "\
module 
=begin =end
=end
 A; end" =>
    [[:module, :A, [:scope]]],

  "module __FILE__::A; b end" =>
    [[:module, [:colon2, [:str, "(string)"], :A], [:scope, [
    :vcall, :b]]]],

  "module __LINE__::A; b end" =>
    [[:module, [:colon2, [:lit, 1], :A], [:scope, [:vcall, :b]]]
    ],

  "module a.b.c.d.e.f::Quux; YYY=534 end" =>
    [[:module, [:colon2, [:call, [:call, [:call, [:call, [:call,
     [:vcall, :a], :b], :c], :d], :e], :f], :Quux], [:scope, [
    :cdecl, :YYY, [:lit, 534]]]]],

  "module self::A include B; c end" =>
    [[:module, [:colon2, [:self], :A], [:scope, [:block, [:fcall,
     :include, [:array, [:const, :B]]], [:vcall, :c]]]]],

  "module self::A; b end" =>
    [[:module, [:colon2, [:self], :A], [:scope, [:vcall, :b]]]],

  "module subject::Builder include T; foo end" =>
    [[:module, [:colon2, [:vcall, :subject], :Builder], [:scope,
     [:block, [:fcall, :include, [:array, [:const, :T]]], [
    :vcall, :foo]]]]],

  "module subject::Builder; foo end" =>
    [[:module, [:colon2, [:vcall, :subject], :Builder], [:scope,
     [:vcall, :foo]]]],

  "module(@a)::Foo; Bar=111 end" =>
    [[:module, [:colon2, [:ivar, :@a], :Foo], [:scope, [:cdecl,
     :Bar, [:lit, 111]]]]],

  "module::A::B::Special::Extend C end" =>
    [[:module, [:colon2, [:colon2, [:colon2, [:colon3, :A], :B],
     :Special], :Extend], [:scope, [:const, :C]]]],

  "module@x::Foo; Bar=1 end" =>
    [[:module, [:colon2, [:ivar, :@x], :Foo], [:scope, [:cdecl,
     :Bar, [:lit, 1]]]]],

  "module[Array][0]::Foo; Bazz=556 end" =>
    [[:module, [:colon2, [:call, [:array, [:const, :Array]], :[], [
    :array, [:lit, 0]]], :Foo], [:scope, [:cdecl, :Bazz, [:lit,
     556]]]]],

  "\
module
#=begin
#=end
A::
#=begin
#=end
B; end\

" =>
    [[:module, [:colon2, [:const, :A], :B], [:scope]]],

  "\
module
=begin
 foo
=end
A; end\

" =>
    [[:module, :A, [:scope]]],

  "\
module
=begin
=end
A::
=begin
=end
B; end\

" =>
    [[:module, [:colon2, [:const, :A], :B], [:scope]]],

  "\
module
A; end" =>
    [[:module, :A, [:scope]]],

  "\
module
__END__
::Foo; end" =>
    SyntaxError.new("(string):2: syntax error, unexpected $end\n__END__\n ^"),

  "m{|((a,)),j| }" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:masgn, [:array, [
    :masgn, [:array, [:dasgn_curr, :a]], nil, nil]], nil, nil], [
    :dasgn_curr, :j]], nil, nil]]],

  "m{|((a,),),j| }" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:masgn, [:array, [
    :masgn, [:array, [:dasgn_curr, :a]], nil, nil]], nil, nil], [
    :dasgn_curr, :j]], nil, nil]]],

  "m{|(*h)| }" =>
    [[:iter, [:fcall, :m], [:masgn, nil, [:dasgn_curr, :h], nil]
    ]],

  "m{|(a,)| }" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:dasgn_curr, :a]],
     nil, nil]]],

  "m{|(a,b)| }" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:dasgn_curr, :a], [
    :dasgn_curr, :b]], nil, nil]]],

  "m{|(a[*b],)| c}" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:attrasgn, [:vcall,
     :a], :[]=, [:splat, [:vcall, :b]]]], nil, nil], [:vcall, :c]
    ]],

  "m{|(a[],)| c }" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:attrasgn, [:vcall,
     :a], :[]=]], nil, nil], [:vcall, :c]]],

  "m{|(a[b=>c,*d,)]| e}" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\nm{|(a[b=>c,*d,)]| e}\n            ^"),
  "m{|(a[b=>c],)| d}" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:attrasgn, [:vcall,
     :a], :[]=, [:array, [:hash, [:vcall, :b], [:vcall, :c]]]]],
     nil, nil], [:vcall, :d]]],

  "m{|(a[b],)| c }" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:attrasgn, [:vcall,
     :a], :[]=, [:array, [:vcall, :b]]]], nil, nil], [:vcall, :c]
    ]],

  "m{|(a[b],t)| c }" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:attrasgn, [:vcall,
     :a], :[]=, [:array, [:vcall, :b]]], [:dasgn_curr, :t]], nil,
     nil], [:vcall, :c]]],

  "m{|(a[y,z,*b],)| c}" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:attrasgn, [:vcall,
     :a], :[]=, [:argscat, [:array, [:vcall, :y], [:vcall, :z]],
     [:vcall, :b]]]], nil, nil], [:vcall, :c]]],

  "m{|(a[z,*b],)| c}" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:attrasgn, [:vcall,
     :a], :[]=, [:argscat, [:array, [:vcall, :z]], [:vcall, :b]]
    ]], nil, nil], [:vcall, :c]]],

  "m{|a,| }" =>
    [[:iter, [:fcall, :m], [:masgn, [:array, [:dasgn_curr, :a]],
     nil, nil]]],

  "m{|a[*b]| c}" =>
    [[:iter, [:fcall, :m], [:attrasgn, [:vcall, :a], :[]=, [
    :splat, [:vcall, :b]]], [:vcall, :c]]],

  "m{|a[b=>c,*d]| e}" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR, expecting ']'\nm{|a[b=>c,*d]| e}\n           ^"),
  "m{|a[b=>c]| d}" =>
    [[:iter, [:fcall, :m], [:attrasgn, [:vcall, :a], :[]=, [
    :array, [:hash, [:vcall, :b], [:vcall, :c]]]], [:vcall, :d]]
    ],

  "m{|a[y,z,*b]| c}" =>
    [[:iter, [:fcall, :m], [:attrasgn, [:vcall, :a], :[]=, [
    :argscat, [:array, [:vcall, :y], [:vcall, :z]], [:vcall, :b]
    ]], [:vcall, :c]]],

  "m{|a[z,*b]| c}" =>
    [[:iter, [:fcall, :m], [:attrasgn, [:vcall, :a], :[]=, [
    :argscat, [:array, [:vcall, :z]], [:vcall, :b]]], [:vcall, :c]
    ]],

  "m{|p ()[b]| }" =>
    [[:iter, [:fcall, :m], [:attrasgn, [:fcall, :p], :[]=, [
    :array, [:vcall, :b]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "m{|p (1)[b]| }" =>
    [[:iter, [:fcall, :m], [:attrasgn, [:fcall, :p, [:array, [
    :lit, 1]]], :[]=, [:array, [:vcall, :b]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "m{|p (1,2)[b]| }" =>
    [[:iter, [:fcall, :m], [:attrasgn, [:fcall, :p, [:array, [
    :lit, 1], [:lit, 2]]], :[]=, [:array, [:vcall, :b]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "m{|p (1..10).method(:each)[*b]| c}" =>
    [[:iter, [:fcall, :m], [:attrasgn, [:call, [:fcall, :p, [
    :array, [:lit, 1..10]]], :method, [:array, [:lit, :each]]],
     :[]=, [:splat, [:vcall, :b]]], [:vcall, :c]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "n = o ? begin end : l" =>
    [[:lasgn, :n, [:if, [:vcall, :o], [:nil], [:vcall, :l]]]],

  "nc=(nextchar unless eof?)" =>
    [[:lasgn, :nc, [:if, [:fcall, :eof?], nil, [:vcall, :nextchar]
    ]]],

  "next" => [[:next]],
  "next * @com_disk" =>
    [[:next, [:svalue, [:splat, [:ivar, :@com_disk]]]]],

  "next *a " => [[:next, [:svalue, [:splat, [:vcall, :a]]]]],
  "next *c" => [[:next, [:svalue, [:splat, [:vcall, :c]]]]],
  "next - @com_disk" =>
    [[:next, [:call, [:ivar, :@com_disk], :-@]]],

  "next 1" => [[:next, [:lit, 1]]],
  "next 1,*b" =>
    [[:next, [:argscat, [:array, [:lit, 1]], [:vcall, :b]]]],

  "next 1,2" => [[:next, [:array, [:lit, 1], [:lit, 2]]]],
  "next 1,2,*c" =>
    [[:next, [:argscat, [:array, [:lit, 1], [:lit, 2]], [:vcall,
     :c]]]],

  "next 1,2,3" =>
    [[:next, [:array, [:lit, 1], [:lit, 2], [:lit, 3]]]],

  "next ::A" => [[:next, [:colon3, :A]]],
  "next a rescue b" =>
    [[:rescue, [:next, [:vcall, :a]], [:resbody, nil, [:vcall, :b]
    ]]],

  "next a,b,*c do d end" =>
    [[:next, [:argscat, [:array, [:vcall, :a], [:vcall, :b]], [
    :iter, [:fcall, :c], nil, [:vcall, :d]]]]],

  "next b,*c" =>
    [[:next, [:argscat, [:array, [:vcall, :b]], [:vcall, :c]]]],

  "next ~1" => [[:next, [:call, [:lit, 1], :~]]],
  "next()" => [[:next, [:nil]]],
  "next(1)" => [[:next, [:lit, 1]]],
  "next(a and b)" => [[:next, [:and, [:vcall, :a], [:vcall, :b]]]],
  "next[1]" => [[:next, [:array, [:lit, 1]]]],
  "nil" => [[:nil]],
  "nil;a" =>
    [[:block, [:nil], [:vcall, :a]],
    {:warnings=>["(string):1: warning: useless use of nil in void context"]}],

  "ni{|b,(c,*d),*e| }" =>
    [[:iter, [:fcall, :ni], [:masgn, [:array, [:dasgn_curr, :b],
     [:masgn, [:array, [:dasgn_curr, :c]], [:dasgn_curr, :d], nil]
    ], [:dasgn_curr, :e], nil]]],

  "ni{|b,(c,d),*e| }" =>
    [[:iter, [:fcall, :ni], [:masgn, [:array, [:dasgn_curr, :b],
     [:masgn, [:array, [:dasgn_curr, :c], [:dasgn_curr, :d]], nil,
     nil]], [:dasgn_curr, :e], nil]]],

  "ni{|b,(c,d),e| }" =>
    [[:iter, [:fcall, :ni], [:masgn, [:array, [:dasgn_curr, :b],
     [:masgn, [:array, [:dasgn_curr, :c], [:dasgn_curr, :d]], nil,
     nil], [:dasgn_curr, :e]], nil, nil]]],

  "ni{|b,(c.f,*d),*e| }" =>
    [[:iter, [:fcall, :ni], [:masgn, [:array, [:dasgn_curr, :b],
     [:masgn, [:array, [:attrasgn, [:vcall, :c], :f=]], [
    :dasgn_curr, :d], nil]], [:dasgn_curr, :e], nil]]],

  "ni{|b,(c.f,g[h],*d),*e| }" =>
    [[:iter, [:fcall, :ni], [:masgn, [:array, [:dasgn_curr, :b],
     [:masgn, [:array, [:attrasgn, [:vcall, :c], :f=], [:attrasgn, [
    :vcall, :g], :[]=, [:array, [:vcall, :h]]]], [:dasgn_curr,
     :d], nil]], [:dasgn_curr, :e], nil]]],

  "ni{|b,(c[f],*d),*e| }" =>
    [[:iter, [:fcall, :ni], [:masgn, [:array, [:dasgn_curr, :b],
     [:masgn, [:array, [:attrasgn, [:vcall, :c], :[]=, [:array, [
    :vcall, :f]]]], [:dasgn_curr, :d], nil]], [:dasgn_curr, :e],
     nil]]],

  "ni{|b,c,*d| }" =>
    [[:iter, [:fcall, :ni], [:masgn, [:array, [:dasgn_curr, :b],
     [:dasgn_curr, :c]], [:dasgn_curr, :d], nil]]],

  "ni{|b,c| }" =>
    [[:iter, [:fcall, :ni], [:masgn, [:array, [:dasgn_curr, :b],
     [:dasgn_curr, :c]], nil, nil]]],

  "ni{|b.c,d.e,*f| }" =>
    [[:iter, [:fcall, :ni], [:masgn, [:array, [:attrasgn, [
    :vcall, :b], :c=], [:attrasgn, [:vcall, :d], :e=]], [
    :dasgn_curr, :f], nil]]],

  "ni{|b.c,d[e],*f| }" =>
    [[:iter, [:fcall, :ni], [:masgn, [:array, [:attrasgn, [
    :vcall, :b], :c=], [:attrasgn, [:vcall, :d], :[]=, [:array, [
    :vcall, :e]]]], [:dasgn_curr, :f], nil]]],

  "ni{|b| }" => [[:iter, [:fcall, :ni], [:dasgn_curr, :b]]],
  "ni{|| }" => [[:iter, [:fcall, :ni], 0]],
  "not (/a..b/)" =>
    [[:not, [:match, [:lit, /a..b/]]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "not (/a..b/) and c" =>
    [[:and, [:not, [:match, [:lit, /a..b/]]], [:vcall, :c]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "not (a..b)" => [[:not, [:flip2, [:vcall, :a], [:vcall, :b]]]],
  "not (a..b) and c" =>
    [[:and, [:not, [:flip2, [:vcall, :a], [:vcall, :b]]], [
    :vcall, :c]]],

  "not (z and (/a..b/) and c)" =>
    [[:not, [:and, [:vcall, :z], [:and, [:lit, /a..b/], [:vcall,
     :c]]]]],

  "not (z and (/a..b/))" =>
    [[:not, [:and, [:vcall, :z], [:lit, /a..b/]]]],

  "not (z and (a..b) and c)" =>
    [[:not, [:and, [:vcall, :z], [:and, [:dot2, [:vcall, :a], [
    :vcall, :b]], [:vcall, :c]]]]],

  "not (z and (a..b))" =>
    [[:not, [:and, [:vcall, :z], [:dot2, [:vcall, :a], [:vcall,
     :b]]]]],

  "not (z and /a..b/ and c)" =>
    [[:not, [:and, [:vcall, :z], [:and, [:match, [:lit, /a..b/]
    ], [:vcall, :c]]]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "not (z and /a..b/)" =>
    [[:not, [:and, [:vcall, :z], [:match, [:lit, /a..b/]]]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "not (z and a..b and c)" =>
    [[:not, [:and, [:vcall, :z], [:and, [:flip2, [:vcall, :a], [
    :vcall, :b]], [:vcall, :c]]]]],

  "not (z and a..b)" =>
    [[:not, [:and, [:vcall, :z], [:flip2, [:vcall, :a], [:vcall,
     :b]]]]],

  "not /a...b/" =>
    [[:not, [:match, [:lit, /a...b/]]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "not /a..b/" =>
    [[:not, [:match, [:lit, /a..b/]]],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "not a and b" => [[:and, [:not, [:vcall, :a]], [:vcall, :b]]],
  "not a...b" => [[:not, [:flip3, [:vcall, :a], [:vcall, :b]]]],
  "not a..b" => [[:not, [:flip2, [:vcall, :a], [:vcall, :b]]]],
  "not begin end" => [[:not, [:nil]]],
  "not begin; a; rescue b; end" =>
    [[:not, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]]]],

  "not z and (/a..b/)" =>
    [[:and, [:not, [:vcall, :z]], [:lit, /a..b/]]],

  "not z and (/a..b/) and c" =>
    [[:and, [:not, [:vcall, :z]], [:and, [:lit, /a..b/], [:vcall,
     :c]]]],

  "not z and (a..b)" =>
    [[:and, [:not, [:vcall, :z]], [:dot2, [:vcall, :a], [:vcall,
     :b]]]],

  "not z and (a..b) and c" =>
    [[:and, [:not, [:vcall, :z]], [:and, [:dot2, [:vcall, :a], [
    :vcall, :b]], [:vcall, :c]]]],

  "not z and /a..b/" =>
    [[:and, [:not, [:vcall, :z]], [:lit, /a..b/]]],

  "not z and /a..b/ and c" =>
    [[:and, [:not, [:vcall, :z]], [:and, [:lit, /a..b/], [:vcall,
     :c]]]],

  "not z and a..b" =>
    [[:and, [:not, [:vcall, :z]], [:dot2, [:vcall, :a], [:vcall,
     :b]]]],

  "not z and a..b and c" =>
    [[:and, [:not, [:vcall, :z]], [:and, [:dot2, [:vcall, :a], [
    :vcall, :b]], [:vcall, :c]]]],

  "n||u n" => SyntaxError.new("(string):1: syntax error, unexpected tIDENTIFIER, expecting kDO or '{' or '('\nn||u n\n      ^"),
  "o & begin end" => [[:call, [:vcall, :o], :&, [:array, [:nil]]]],
  "o && begin end" => [[:and, [:vcall, :o], [:nil]]],
  "o ? begin end : l" =>
    [[:if, [:vcall, :o], [:nil], [:vcall, :l]]],

  "o and begin end" => [[:and, [:vcall, :o], [:nil]]],
  "o if begin end" => [[:if, [:nil], [:vcall, :o], nil]],
  "o or begin end" => [[:or, [:vcall, :o], [:nil]]],
  "o unless begin end" => [[:if, [:nil], nil, [:vcall, :o]]],
  "o until begin end" => [[:until, [:nil], [:vcall, :o], true]],
  "o while begin end" => [[:while, [:nil], [:vcall, :o], true]],
  "o | begin end" => [[:call, [:vcall, :o], :|, [:array, [:nil]]]],
  "o || begin end" => [[:or, [:vcall, :o], [:nil]]],
  "p" => [[:vcall, :p]],
  "p   BLACK = 1" =>
    [[:fcall, :p, [:array, [:cdecl, :BLACK, [:lit, 1]]]]],

  "p   EMPTY = 0" =>
    [[:fcall, :p, [:array, [:cdecl, :EMPTY, [:lit, 0]]]]],

  "p   WHITE = - BLACK" =>
    [[:fcall, :p, [:array, [:cdecl, :WHITE, [:call, [
    :const, :BLACK], :-@]]]]],

  "p \"\#$a \#@b \#@@c \#{$a+@b+@@c}\"" =>
    [[:fcall, :p, [:array, [:dstr, "", [:evstr, [:gvar, :$a]], [
    :str, " "], [:evstr, [:ivar, :@b]], [:str, " "], [:evstr, [
    :cvar, :@@c]], [:str, " "], [:evstr, [:call, [:call, [:gvar,
     :$a], :+, [:array, [:ivar, :@b]]], :+, [:array, [:cvar, :@@c]
    ]]]]]]],

  "\
p \"\#$a \#@b \#@@c
d e f
\#$a \#@b \#@@c
\"\

" =>
    [[:fcall, :p, [:array, [:dstr, "", [:evstr, [:gvar, :$a]], [
    :str, " "], [:evstr, [:ivar, :@b]], [:str, " "], [:evstr, [
    :cvar, :@@c]], [:str, "\nd e f\n"], [:evstr, [:gvar, :$a]], [
    :str, " "], [:evstr, [:ivar, :@b]], [:str, " "], [:evstr, [
    :cvar, :@@c]], [:str, "\n"]]]]],

  "p \"#(1)\"" => [[:fcall, :p, [:array, [:str, "#(1)"]]]],
  "p \"\#{$!.class}\"" =>
    [[:fcall, :p, [:array, [:dstr, "", [:evstr, [:call, [:gvar,
     :$!], :class]]]]]],

  "\
p \"\#{<<foobar3}\"
bim
baz
bof
foobar3\

" =>
    [[:fcall, :p, [:array, [:str, "bim\nbaz\nbof\n"]]]],

  "\
p \"\#{<<kekerz}\#{\"foob\"
zimpler
kekerz
}\"\

" =>
    [[:fcall, :p, [:array, [:str, "zimpler\nfoob"]]]],

  "\
p \"\#{__FILE__}\"
p \"\#{__LINE__}\"" =>
    [[:block, [:fcall, :p, [:array, [:str, "(string)"]]], [
    :fcall, :p, [:array, [:dstr, "", [:evstr, [:lit, 2]]]]]]],

  "p \"\#{}\"" => [[:fcall, :p, [:array, [:dstr, "", [:evstr]]]]],
  "\
p \"123
456
789\\nabc
def\\nghi
\"
\n" =>
    [[:fcall, :p, [:array, [:str, "123\n456\n789\nabc\ndef\nghi\n"]
    ]]],

  "p \"Hi, my name is \#{\"Slim \#{(4)<2?\"Whitman\":\"Shady\"}\
 \"}.\"" =>
    [[:fcall, :p, [:array, [:dstr, "Hi, my name is Slim ", [
    :evstr, [:if, [:call, [:lit, 4], :<, [:array, [:lit, 2]]], [
    :str, "Whitman"], [:str, "Shady"]]], [:str, " "], [:str, "."]
    ]]]],

  "p \"Hi, my name is \#{\"Slim \#{(4)>2?\"Whitman\":\"Shady\"}\
 \"}.\"" =>
    [[:fcall, :p, [:array, [:dstr, "Hi, my name is Slim ", [
    :evstr, [:if, [:call, [:lit, 4], :>, [:array, [:lit, 2]]], [
    :str, "Whitman"], [:str, "Shady"]]], [:str, " "], [:str, "."]
    ]]]],

  "\
p \"
\"\

" =>
    [[:fcall, :p, [:array, [:str, "\n"]]]],

  "p \"\\\"\"" => [[:fcall, :p, [:array, [:str, "\""]]]],
  "p \"\\\#$a \\\#@b \\\#@@c \\\#{$a+@b+@@c}\"" =>
    [[:fcall, :p, [:array, [:str, "\#$a \#@b \#@@c \#{$a+@b+@@c}"]
    ]]],

  "p \"\\n\"" => [[:fcall, :p, [:array, [:str, "\n"]]]],
  "p \"\\v\"" => [[:fcall, :p, [:array, [:str, "\v"]]]],
  "p $-j=55" =>
    [[:fcall, :p, [:array, [:gasgn, :$-j, [:lit, 55]]]]],

  "p $10" => [[:fcall, :p, [:array, [:nth_ref, 10]]]],
  "p $1001" => [[:fcall, :p, [:array, [:nth_ref, 1001]]]],
  "p % foo" =>
    [[:call, [:vcall, :p], :%, [:array, [:vcall, :foo]]]],

  "p %(1)" => [[:fcall, :p, [:array, [:str, "1"]]]],
  "p %/p/" => [[:fcall, :p, [:array, [:str, "p"]]]],
  "p %Q[ some [nested] text]" =>
    [[:fcall, :p, [:array, [:str, " some [nested] text"]]]],

  "p %Q[\#$a \#@b \#@@c \#{$a+@b+@@c}]" =>
    [[:fcall, :p, [:array, [:dstr, "", [:evstr, [:gvar, :$a]], [
    :str, " "], [:evstr, [:ivar, :@b]], [:str, " "], [:evstr, [
    :cvar, :@@c]], [:str, " "], [:evstr, [:call, [:call, [:gvar,
     :$a], :+, [:array, [:ivar, :@b]]], :+, [:array, [:cvar, :@@c]
    ]]]]]]],

  "p %Q[<LI><A HREF=\"\#{i[3]}.html\\#\#{i[4]}\">\#{i[0]+i[1]+\
(i[2])}</A>\\n]" =>
    [[:fcall, :p, [:array, [:dstr, "<LI><A HREF=\"", [:evstr, [
    :call, [:vcall, :i], :[], [:array, [:lit, 3]]]], [:str, ".html#"], [
    :evstr, [:call, [:vcall, :i], :[], [:array, [:lit, 4]]]], [
    :str, "\">"], [:evstr, [:call, [:call, [:call, [:vcall, :i],
     :[], [:array, [:lit, 0]]], :+, [:array, [:call, [:vcall, :i],
     :[], [:array, [:lit, 1]]]]], :+, [:array, [:call, [:vcall,
     :i], :[], [:array, [:lit, 2]]]]]], [:str, "</A>\n"]]]]],

  "p %Q[<LI>]" => [[:fcall, :p, [:array, [:str, "<LI>"]]]],
  "p %Q[\\\"]" => [[:fcall, :p, [:array, [:str, "\""]]]],
  "p %Q[\\\#$a \\\#@b \\\#@@c \\\#{$a+@b+@@c}]" =>
    [[:fcall, :p, [:array, [:str, "\#$a \#@b \#@@c \#{$a+@b+@@c}"]
    ]]],

  "p %W\"\#$a \#@b \#@@c \#{$a+@b+@@c}\"" =>
    [[:fcall, :p, [:array, [:array, [:dstr, "", [:evstr, [:gvar,
     :$a]]], [:dstr, "", [:evstr, [:ivar, :@b]]], [:dstr, "", [
    :evstr, [:cvar, :@@c]]], [:dstr, "", [:evstr, [:call, [:call, [
    :gvar, :$a], :+, [:array, [:ivar, :@b]]], :+, [:array, [
    :cvar, :@@c]]]]]]]]],

  "p %W\"\\\#$a \\\#@b \\\#@@c \\\#{$a+@b+@@c}\"" =>
    [[:fcall, :p, [:array, [:array, [:str, "\#$a"], [:str, "\#@b"], [
    :str, "\#@@c"], [:str, "\#{$a+@b+@@c}"]]]]],

  "\
p %W/
/\

" =>
    [[:fcall, :p, [:array, [:zarray]]]],

  "p %W/\\n/" => [[:fcall, :p, [:array, [:array, [:str, "\n"]]]]],
  "p %\\hah, \#{backslash} as string delimiter\\" =>
    [[:fcall, :p, [:array, [:dstr, "hah, ", [:evstr, [:vcall,
     :backslash]], [:str, " as string delimiter"]]]]],

  "p %\\hah, backslash as string delimiter\\" =>
    [[:fcall, :p, [:array, [:str, "hah, backslash as string delimiter"]
    ]]],

  "\
p %r{\\/$}
p %r~<!include:([\\/\\w\\.\\-]+)>~m
\np [].push *[1,2,3]
p /\\n/
\n$a=1
@b=2
@@c=3
p(/\\\#$a \\\#@b \\\#@@c \\\#{$a+@b+@@c}/)
\n
class Foo
attr :foo,true
end
f=Foo.new
p f.foo
p f.foo=9
p f.foo =19
p f.foo= 29
p f.foo = 39
p f.foo\

" =>
    [[:block, [:fcall, :p, [:array, [:lit, /\/$/]]], [:fcall, :p, [
    :array, [:lit, /<!include:([\/\w\.\-]+)>/m]]], [:fcall, :p, [
    :array, [:call, [:zarray], :push, [:splat, [:array, [:lit, 1], [
    :lit, 2], [:lit, 3]]]]]], [:fcall, :p, [:array, [:lit, /\n/]
    ]], [:gasgn, :$a, [:lit, 1]], [:iasgn, :@b, [:lit, 2]], [
    :cvdecl, :@@c, [:lit, 3]], [:fcall, :p, [:array, [
    :lit, /\#$a \#@b \#@@c \#{$a+@b+@@c}/]]], [:class, :Foo, nil, [
    :scope, [:fcall, :attr, [:array, [:lit, :foo], [:true]]]]], [
    :lasgn, :f, [:call, [:const, :Foo], :new]], [:fcall, :p, [
    :array, [:call, [:lvar, :f], :foo]]], [:fcall, :p, [:array, [
    :attrasgn, [:lvar, :f], :foo=, [:array, [:lit, 9]]]]], [
    :fcall, :p, [:array, [:attrasgn, [:lvar, :f], :foo=, [:array, [
    :lit, 19]]]]], [:fcall, :p, [:array, [:attrasgn, [:lvar, :f],
     :foo=, [:array, [:lit, 29]]]]], [:fcall, :p, [:array, [
    :attrasgn, [:lvar, :f], :foo=, [:array, [:lit, 39]]]]], [
    :fcall, :p, [:array, [:call, [:lvar, :f], :foo]]]],
    {:warnings=>["(string):4: warning: `*' interpreted as argument prefix", "(string):4: warning: parenthesize argument(s) for future version", "(string):5: warning: ambiguous first argument; put parentheses or even spaces", "(string):10: warning: regexp has invalid interval", "(string):10: warning: regexp has `}' without escape"]}],

  "p %s jim ;" => [[:fcall, :p, [:array, [:lit, :jim]]]],
  "p %s\"jim\";" => [[:fcall, :p, [:array, [:lit, :jim]]]],
  "p %s'jim';" => [[:fcall, :p, [:array, [:lit, :jim]]]],
  "p %s(jim);" => [[:fcall, :p, [:array, [:lit, :jim]]]],
  "p %s/jim/;" => [[:fcall, :p, [:array, [:lit, :jim]]]],
  "p %s<jim>;" => [[:fcall, :p, [:array, [:lit, :jim]]]],
  "p %s[jim];" => [[:fcall, :p, [:array, [:lit, :jim]]]],
  "p %s`jim`;" => [[:fcall, :p, [:array, [:lit, :jim]]]],
  "p %s{jim};" => [[:fcall, :p, [:array, [:lit, :jim]]]],
  "p %s{symbol}" => [[:fcall, :p, [:array, [:lit, :symbol]]]],
  "p %w\"\#$a \#@b \#@@c \#{$a+@b+@@c}\"" =>
    [[:fcall, :p, [:array, [:array, [:str, "\#$a"], [:str, "\#@b"], [
    :str, "\#@@c"], [:str, "\#{$a+@b+@@c}"]]]]],

  "p %w\"\\\#$a \\\#@b \\\#@@c \\\#{$a+@b+@@c}\"" =>
    [[:fcall, :p, [:array, [:array, [:str, "\\\#$a"], [
    :str, "\\\#@b"], [:str, "\\\#@@c"], [:str, "\\\#{$a+@b+@@c}"]
    ]]]],

  "\
p %w/
/\

" =>
    [[:fcall, :p, [:array, [:zarray]]]],

  "p %w/\\n/" => [[:fcall, :p, [:array, [:array, [:str, "\\n"]]]]],
  "p %w[\\]]" => [[:fcall, :p, [:array, [:array, [:str, "]"]]]]],
  "p %w[\\\\]" => [[:fcall, :p, [:array, [:array, [:str, "\\"]]]]],
  "\
p %w[a b c
     d e f]\

" =>
    [[:fcall, :p, [:array, [:array, [:str, "a"], [:str, "b"], [
    :str, "c"], [:str, "d"], [:str, "e"], [:str, "f"]]]]],

  "\
p %w[a b c\\n
     d e f]\

" =>
    [[:fcall, :p, [:array, [:array, [:str, "a"], [:str, "b"], [
    :str, "c\\n"], [:str, "d"], [:str, "e"], [:str, "f"]]]]],

  "p %{{{{\#{\"}}}\"}}}}}" =>
    [[:fcall, :p, [:array, [:str, "{{{}}}}}}"]]]],

  "p '\#$a \#@b \#@@c \#{$a+@b+@@c}'" =>
    [[:fcall, :p, [:array, [:str, "\#$a \#@b \#@@c \#{$a+@b+@@c}"]
    ]]],

  "\
p '
'\

" =>
    [[:fcall, :p, [:array, [:str, "\n"]]]],

  "p '\\\#$a \\\#@b \\\#@@c \\\#{$a+@b+@@c}'" =>
    [[:fcall, :p, [:array, [:str, "\\\#$a \\\#@b \\\#@@c \\\#{$a+@b+@@c}"]
    ]]],

  "p '\\n'" => [[:fcall, :p, [:array, [:str, "\\n"]]]],
  "p (&a)" =>
    [[:block_pass, [:vcall, :a], [:fcall, :p]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "p ()" =>
    [[:fcall, :p],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "p ().method(:each)" => SyntaxError.new("(string):1: syntax error, unexpected '.', expecting $end\np ().method(:each)\n     ^"),
  "p (*a)" =>
    [[:fcall, :p, [:splat, [:vcall, :a]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "p (*a=c)" =>
    [[:fcall, :p, [:splat, [:lasgn, :a, [:vcall, :c]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "p (1),*a=c" =>
    [[:fcall, :p, [:argscat, [:array, [:lit, 1]], [:lasgn, :a, [
    :vcall, :c]]]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "p (1).c" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :c]]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "p (1,2)    " =>
    [[:fcall, :p, [:array, [:lit, 1], [:lit, 2]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "p (1,2,3)" =>
    [[:fcall, :p, [:array, [:lit, 1], [:lit, 2], [:lit, 3]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "p (1-2).nil?     " =>
    [[:fcall, :p, [:array, [:call, [:call, [:lit, 1], :-, [:array, [
    :lit, 2]]], :nil?]]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "p (1..10).method(:each)" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1..10], :method, [
    :array, [:lit, :each]]]]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "p (1..10).method(:each)[z,*b],z=c,y" =>
    [[:fcall, :p, [:array, [:call, [:call, [:lit, 1..10], :method, [
    :array, [:lit, :each]]], :[], [:argscat, [:array, [:vcall, :z]
    ], [:vcall, :b]]], [:lasgn, :z, [:vcall, :c]], [:vcall, :y]]
    ],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "p (1..10,1).method(:each)" => SyntaxError.new("(string):1: syntax error, unexpected '.', expecting $end\np (1..10,1).method(:each)\n            ^"),
  "p (1.10).me,z=c" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1.1], :me], [:lasgn, :z, [
    :vcall, :c]]]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "\
p (M.
__END__
)" =>
    SyntaxError.new("(string):2: syntax error, unexpected $end\n__END__\n ^"),

  "p (Module.instance_methods - Object.instance_methods).sort \
 " =>
    [[:fcall, :p, [:array, [:call, [:call, [:call, [
    :const, :Module], :instance_methods], :-, [:array, [:call, [
    :const, :Object], :instance_methods]]], :sort]]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression"]}],

  "\
p (
__END__
)" =>
    SyntaxError.new("(string):2: syntax error, unexpected $end, expecting ')'\n__END__\n ^"),

  "p (a,b=c,d); a +h" =>
    [[:block, [:fcall, :p, [:array, [:vcall, :a], [:lasgn, :b, [
    :vcall, :c]], [:vcall, :d]]], [:fcall, :a, [:array, [:call, [
    :vcall, :h], :+@]]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses", "(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "p *[]" =>
    [[:fcall, :p, [:splat, [:zarray]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "p *a=c" =>
    [[:fcall, :p, [:splat, [:lasgn, :a, [:vcall, :c]]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "p *b=c" =>
    [[:fcall, :p, [:splat, [:lasgn, :b, [:vcall, :c]]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "p +(4)" =>
    [[:fcall, :p, [:array, [:call, [:lit, 4], :+@]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "p -(4)" =>
    [[:fcall, :p, [:array, [:call, [:lit, 4], :-@]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "p //e" =>
    [[:fcall, :p, [:array, [:lit, //n]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "p //n" =>
    [[:fcall, :p, [:array, [:lit, //]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "p //s" =>
    [[:fcall, :p, [:array, [:lit, //n]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "p //u" =>
    [[:fcall, :p, [:array, [:lit, //]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "p 0.1" => [[:fcall, :p, [:array, [:lit, 0.1]]]],
  "p 0.8" => [[:fcall, :p, [:array, [:lit, 0.8]]]],
  "p 0.9" => [[:fcall, :p, [:array, [:lit, 0.9]]]],
  "p 0123456701" => [[:fcall, :p, [:array, [:lit, 21913025]]]],
  "p 0b0100011001" => [[:fcall, :p, [:array, [:lit, 281]]]],
  "p 0o" => [[:fcall, :p, [:array, [:lit, 0]]]],
  "p 0o123456701" => [[:fcall, :p, [:array, [:lit, 21913025]]]],
  "p 0x123456789abcdefABCDEF01" =>
    [[:fcall, :p, [:array, [:lit, 352125166730063720919985921]]]
    ],

  "p 0x80" => [[:fcall, :p, [:array, [:lit, 128]]]],
  "p 1.45000000000000000000000000000000000000001" =>
    [[:fcall, :p, [:array, [:lit, 1.45]]]],

  "p 1;2;p 1" =>
    [[:block, [:fcall, :p, [:array, [:lit, 1]]], [:lit, 2], [
    :fcall, :p, [:array, [:lit, 1]]]],
    {:warnings=>["(string):1: warning: useless use of a literal in void context"]}],

  "p 9.999999999999999999999999999999999999999999" =>
    [[:fcall, :p, [:array, [:lit, 10.0]]]],

  "p\
 9.999999999999999999999999999999999999999999e999999999999999999999999999" =>
    [[:fcall, :p, [:array, [:lit, Infinity]]],
    {:warnings=>["(string):1: warning: Float 9.999999999999999999999999999999999999999999e999999999999999999999999999 out of range"]}],

  "p 99 / 3" =>
    [[:fcall, :p, [:array, [:call, [:lit, 99], :/, [:array, [
    :lit, 3]]]]]],

  "p :\"[]\"" => [[:fcall, :p, [:array, [:lit, :[]]]]],
  "p :\"jim\";" => [[:fcall, :p, [:array, [:lit, :jim]]]],
  "p :$1" => [[:fcall, :p, [:array, [:lit, :$1]]]],
  "p :$98349576875974523789734582394578" =>
    [[:fcall, :p, [:array, [:lit, :$98349576875974523789734582394578]
    ]]],

  "p :'\\\\'" => [[:fcall, :p, [:array, [:lit, :"\\"]]]],
  "p :'jim';" => [[:fcall, :p, [:array, [:lit, :jim]]]],
  "p :+" => [[:fcall, :p, [:array, [:lit, :+]]]],
  "p :`" => [[:fcall, :p, [:array, [:lit, :`]]]],
  "p :p" => [[:fcall, :p, [:array, [:lit, :p]]]],
  "p :p8" => [[:fcall, :p, [:array, [:lit, :p8]]]],
  "p :~@" => [[:fcall, :p, [:array, [:lit, :~]]]],
  "\
p <<\"..end ..\"
cbkvjb
vb;lkxcvkbxc
vxlc;kblxckvb
xcvblcvb
..end ..\

" =>
    [[:fcall, :p, [:array, [
    :str, "cbkvjb\nvb;lkxcvkbxc\nvxlc;kblxckvb\nxcvblcvb\n"]]]],

  "\
p <<'END'
hgjfg
END\

" =>
    [[:fcall, :p, [:array, [:str, "hgjfg\n"]]]],

  "\
p <<'p'
\\\np
p\

" =>
    [[:block, [:fcall, :p, [:array, [:str, "\\\n"]]], [:vcall, :p]
    ]],

  "\
p <<'p'
\\n\\t\\r\\v\\
p
p\

" =>
    [[:block, [:fcall, :p, [:array, [:str, "\\n\\t\\r\\v\\\n"]]
    ], [:vcall, :p]]],

  "\
p <<'p'
\\n\\t\\r\\v\\\\
p" =>
    [[:fcall, :p, [:array, [:str, "\\n\\t\\r\\v\\\\\n"]]]],

  "\
p <<'p'
\\n\\t\\r\\v\\\\
p\

" =>
    [[:fcall, :p, [:array, [:str, "\\n\\t\\r\\v\\\\\n"]]]],

  "\
p <<-heredoc \"x y z\" and 5
       a b c
     heredoc" =>
    [[:and, [:fcall, :p, [:array, [:str, "       a b c\nx y z"]]
    ], [:lit, 5]]],

  "\
p <<-what
     ? that's
  what\

" =>
    [[:fcall, :p, [:array, [:str, "     ? that's\n"]]]],

  "\
p <<Class
  zzzz
Class\

" =>
    [[:fcall, :p, [:array, [:str, "  zzzz\n"]]]],

  "\
p <<a
dkflg
flk
a\

" =>
    [[:fcall, :p, [:array, [:str, "dkflg\nflk\n"]]]],

  "\
p <<f
f
\n" =>
    [[:fcall, :p, [:array, [:str, ""]]]],

  "\
p <<ggg; def
kleegarts() p 'kkkkkkk' end
dfgdgfdf
ggg
koomblatz!() p 'jdkfsk' end" =>
    [[:block, [:fcall, :p, [:array, [:str, "kleegarts() p 'kkkkkkk' end\ndfgdgfdf\n"]]], [
    :defn, :koomblatz!, [:scope, [:block, [:args], [:fcall, :p, [
    :array, [:str, "jdkfsk"]]]]]]]],

  "\
p <<ggg; def
kleegarts() p 'kkkkkkk' end
dfgdgfdf
ggg
koomblatz!() p 'jdkfsk' end
koomblatz!\

" =>
    [[:block, [:fcall, :p, [:array, [:str, "kleegarts() p 'kkkkkkk' end\ndfgdgfdf\n"]]], [
    :defn, :koomblatz!, [:scope, [:block, [:args], [:fcall, :p, [
    :array, [:str, "jdkfsk"]]]]]], [:fcall, :koomblatz!]]],

  "\
p <<p
\\\np
p\

" =>
    [[:fcall, :p, [:array, [:str, "p\n"]]]],

  "\
p <<p
\\\nsdfgd
p\

" =>
    [[:fcall, :p, [:array, [:str, "sdfgd\n"]]]],

  "\
p <<p
\\n\\t\\r\\v\\\\
p\

" =>
    [[:fcall, :p, [:array, [:str, "\n\t\r\v\\\n"]]]],

  "\
p <<p
sdfsfdf^^^^@@@
p\

" =>
    [[:fcall, :p, [:array, [:str, "sdfsfdf^^^^@@@\n"]]]],

  "\
p <<stuff+'foobar'.tr('j-l','d-f')
\"more stuff\"
12345678
the quick brown fox jumped over the lazy dog0
stuff\

" =>
    [[:fcall, :p, [:array, [:call, [
    :str, "\"more stuff\"\n12345678\nthe quick brown fox jumped over the lazy dog0\n"], :+, [
    :array, [:call, [:str, "foobar"], :tr, [:array, [:str, "j-l"], [
    :str, "d-f"]]]]]]]],

  "\
p <<stuff+'foobar'.tr('j-l','d-f')
\"more stuff\"
12345678
the quick brown fox jumped over the lazy dog1
stuff\

" =>
    [[:fcall, :p, [:array, [:call, [
    :str, "\"more stuff\"\n12345678\nthe quick brown fox jumped over the lazy dog1\n"], :+, [
    :array, [:call, [:str, "foobar"], :tr, [:array, [:str, "j-l"], [
    :str, "d-f"]]]]]]]],

  "\
p <<stuff+'foobar'.tr('j-l','d-f')
+\"more stuff\"
12345678
the quick brown fox jumped over the lazy dog
stuff\

" =>
    [[:fcall, :p, [:array, [:call, [
    :str, "+\"more stuff\"\n12345678\nthe quick brown fox jumped over the lazy dog\n"], :+, [
    :array, [:call, [:str, "foobar"], :tr, [:array, [:str, "j-l"], [
    :str, "d-f"]]]]]]]],

  "\
p <<stuff+'foobar'.tr('j-l','d-f')\\
+\"more stuff\"
12345678
the quick brown fox jumped over the lazy dog2
stuff\

" =>
    [[:fcall, :p, [:array, [:call, [
    :str, "+\"more stuff\"\n12345678\nthe quick brown fox jumped over the lazy dog2\n"], :+, [
    :array, [:call, [:str, "foobar"], :tr, [:array, [:str, "j-l"], [
    :str, "d-f"]]]]]]]],

  "\
p <<stuff+'foobar'.tr('j-l','d-f')\\
+\"more stuff\"
12345678
the quick brown fox jumped over the lazy dog
stuff" =>
    [[:fcall, :p, [:array, [:call, [
    :str, "+\"more stuff\"\n12345678\nthe quick brown fox jumped over the lazy dog\n"], :+, [
    :array, [:call, [:str, "foobar"], :tr, [:array, [:str, "j-l"], [
    :str, "d-f"]]]]]]]],

  "\
p <<stuff+'foobar'.tr('j-l','d-f')\\
+\"more stuff\"
12345678
the quick brown fox jumped over the lazy dog
stuff
+\"even more\"" =>
    [[:fcall, :p, [:array, [:call, [:call, [
    :str, "+\"more stuff\"\n12345678\nthe quick brown fox jumped over the lazy dog\n"], :+, [
    :array, [:call, [:str, "foobar"], :tr, [:array, [:str, "j-l"], [
    :str, "d-f"]]]]], :+, [:array, [:str, "even more"]]]]]],

  "\
p <<stuff+'foobar'.tr('j-l','d-f')\\
+\"more stuff\"
12345678
the quick brown fox jumped over the lazy dog
stuff
+\"even more\"\

" =>
    [[:fcall, :p, [:array, [:call, [:call, [
    :str, "+\"more stuff\"\n12345678\nthe quick brown fox jumped over the lazy dog\n"], :+, [
    :array, [:call, [:str, "foobar"], :tr, [:array, [:str, "j-l"], [
    :str, "d-f"]]]]], :+, [:array, [:str, "even more"]]]]]],

  "\
p <<stuff+'foobar'.tr('j-l','d-f')\\
+\"more stuff\"
12345678
the quick brown fox jumped over the lazy dog
stuff
\n" =>
    [[:fcall, :p, [:array, [:call, [
    :str, "+\"more stuff\"\n12345678\nthe quick brown fox jumped over the lazy dog\n"], :+, [
    :array, [:call, [:str, "foobar"], :tr, [:array, [:str, "j-l"], [
    :str, "d-f"]]]]]]]],

  "\
p <<stuff+'foobar'\\
+\"more stuff\"
12345678
the quick brown fox jumped over the lazy dog3
stuff\

" =>
    [[:fcall, :p, [:array, [:call, [
    :str, "+\"more stuff\"\n12345678\nthe quick brown fox jumped over the lazy dog3\n"], :+, [
    :array, [:str, "foobar"]]]]]],

  "\
p <<stuff+'foobar'\\
+\"more stuff\"
some stuff
stuff
\np <<stuff+'foobar'+
some stuff
stuff
\"more stuff\"
\np <<stuff+'foobar'+<<baggage+\"baz\"+<<impedimentia+
some stuff
stuff
blah blah
baggage
hic haec hoc
impedimentia
\"more stuff\"
\n\

" =>
    [[:block, [:fcall, :p, [:array, [:call, [
    :str, "+\"more stuff\"\nsome stuff\n"], :+, [:array, [:str,
     "foobar"]]]]], [:fcall, :p, [:array, [:call, [:call, [:str,
     "some stuff\n"], :+, [:array, [:str, "foobar"]]], :+, [
    :array, [:str, "more stuff"]]]]], [:fcall, :p, [:array, [
    :call, [:call, [:call, [:call, [:call, [:str, "some stuff\n"],
     :+, [:array, [:str, "foobar"]]], :+, [:array, [
    :str, "blah blah\n"]]], :+, [:array, [:str, "baz"]]], :+, [
    :array, [:str, "hic haec hoc\n"]]], :+, [:array, [
    :str, "more stuff"]]]]]]],

  "p = k.p m %(1)" =>
    [[:lasgn, :p, [:call, [:vcall, :k], :p, [:array, [:fcall, :m, [
    :array, [:str, "1"]]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p = k.p m %(1) do end" =>
    [[:lasgn, :p, [:iter, [:call, [:vcall, :k], :p, [:array, [
    :fcall, :m, [:array, [:str, "1"]]]]], nil]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p = k::p m %(1)" =>
    [[:lasgn, :p, [:call, [:vcall, :k], :p, [:array, [:fcall, :m, [
    :array, [:str, "1"]]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p = k::p m %(1) do end" =>
    [[:lasgn, :p, [:iter, [:call, [:vcall, :k], :p, [:array, [
    :fcall, :m, [:array, [:str, "1"]]]]], nil]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p = p m %(1)" =>
    [[:lasgn, :p, [:fcall, :p, [:array, [:call, [:vcall, :m], :%, [
    :array, [:lit, 1]]]]]]],

  "p = p m %(1) ,&t" =>
    [[:lasgn, :p, [:block_pass, [:vcall, :t], [:fcall, :p, [
    :array, [:call, [:vcall, :m], :%, [:array, [:lit, 1]]]]]]]],

  "p = p m %(1) ,*t" =>
    [[:lasgn, :p, [:fcall, :p, [:argscat, [:array, [:call, [
    :vcall, :m], :%, [:array, [:lit, 1]]]], [:vcall, :t]]]]],

  "p = p m %(1) ,t" =>
    [[:lasgn, :p, [:fcall, :p, [:array, [:call, [:vcall, :m], :%, [
    :array, [:lit, 1]]], [:vcall, :t]]]]],

  "p = p m %(1) do end" =>
    [[:lasgn, :p, [:iter, [:fcall, :p, [:array, [:call, [:vcall,
     :m], :%, [:array, [:lit, 1]]]]], nil]]],

  "p ?e.%?y" =>
    [[:fcall, :p, [:array, [:call, [:lit, 101], :%, [:array, [
    :lit, 121]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p ?e.&?y" =>
    [[:fcall, :p, [:array, [:call, [:lit, 101], :&, [:array, [
    :lit, 121]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p ?e.**?y" =>
    [[:fcall, :p, [:array, [:call, [:lit, 101], :**, [:array, [
    :lit, 121]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p ?e.*?y" =>
    [[:fcall, :p, [:array, [:call, [:lit, 101], :*, [:array, [
    :lit, 121]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p ?e.+ ?y" =>
    [[:fcall, :p, [:array, [:call, [:lit, 101], :+, [:array, [
    :lit, 121]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p ?e.+?y" =>
    [[:fcall, :p, [:array, [:call, [:lit, 101], :+, [:array, [
    :lit, 121]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p ?e.-?y" =>
    [[:fcall, :p, [:array, [:call, [:lit, 101], :-, [:array, [
    :lit, 121]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p ?e./?y" =>
    [[:fcall, :p, [:array, [:call, [:lit, 101], :/, [:array, [
    :lit, 121]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p ?e.<<?y" =>
    [[:fcall, :p, [:array, [:call, [:lit, 101], :<<, [:array, [
    :lit, 121]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p ?p" => [[:fcall, :p, [:array, [:lit, 112]]]],
  "p Array(\"foo\\nbar\")" =>
    [[:fcall, :p, [:array, [:fcall, :Array, [:array, [
    :str, "foo\nbar"]]]]]],

  "p ENV[\"AmritaCacheDir\"]" =>
    [[:fcall, :p, [:array, [:call, [:const, :ENV], :[], [:array,
     [:str, "AmritaCacheDir"]]]]]],

  "p File" => [[:fcall, :p, [:array, [:const, :File]]]],
  "p Foou.new.[] !false " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:not, [:false]]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p Foou.new.[] $9     " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:nth_ref, 9]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p Foou.new.[] %(9)   " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:str, "9"]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p Foou.new.[] +9     " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:lit, 9]]]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces", "(string):1: warning: parenthesize argument(s) for future version"]}],

  "p Foou.new.[] -9     " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:lit, -9]]]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces", "(string):1: warning: parenthesize argument(s) for future version"]}],

  "p Foou.new.[] /9/    " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:lit, /9/]]]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces", "(string):1: warning: parenthesize argument(s) for future version"]}],

  "\
p Foou.new.[] <<9    #value
foobar
9\

" =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:str, "foobar\n"]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p Foou.new.[] a0     " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:vcall, :a0]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p Foou.new.[] {9} " =>
    [[:fcall, :p, [:array, [:iter, [:call, [:call, [:const, :Foou],
     :new], :[]], nil, [:lit, 9]]]]],

  "p Foou.new.[] ~9     " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:call, [:lit, 9], :~]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p Foou.new.[]!false  " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:not, [:false]]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p Foou.new.[]$9      " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:nth_ref, 9]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p Foou.new.[]%9      " =>
    [[:fcall, :p, [:array, [:call, [:call, [:call, [:const, :Foou],
     :new], :[]], :%, [:array, [:lit, 9]]]]]],

  "p Foou.new.[]+9      " =>
    [[:fcall, :p, [:array, [:call, [:call, [:call, [:const, :Foou],
     :new], :[]], :+, [:array, [:lit, 9]]]]]],

  "p Foou.new.[]-9      " =>
    [[:fcall, :p, [:array, [:call, [:call, [:call, [:const, :Foou],
     :new], :[]], :-, [:array, [:lit, 9]]]]]],

  "p Foou.new.[]/9      " =>
    [[:fcall, :p, [:array, [:call, [:call, [:call, [:const, :Foou],
     :new], :[]], :/, [:array, [:lit, 9]]]]]],

  "p Foou.new.[]<<9     " =>
    [[:fcall, :p, [:array, [:call, [:call, [:call, [:const, :Foou],
     :new], :[]], :<<, [:array, [:lit, 9]]]]]],

  "p Foou.new.[]a0      " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:vcall, :a0]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p Foou.new.[]{9}     " =>
    [[:fcall, :p, [:array, [:iter, [:call, [:call, [:const, :Foou],
     :new], :[]], nil, [:lit, 9]]]]],

  "p Foou.new.[]~9      " =>
    [[:fcall, :p, [:array, [:call, [:call, [:const, :Foou], :new],
     :[], [:array, [:call, [:lit, 9], :~]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "\
p Module.
__END__" =>
    SyntaxError.new("(string):2: syntax error, unexpected $end\n__END__\n ^"),

  "p [1,2,3,]" =>
    [[:fcall, :p, [:array, [:array, [:lit, 1], [:lit, 2], [:lit,
     3]]]]],

  "\
p [1,2,3][1]
begin p[1]
rescue
end\

" =>
    [[:block, [:fcall, :p, [:array, [:call, [:array, [:lit, 1], [
    :lit, 2], [:lit, 3]], :[], [:array, [:lit, 1]]]]], [:rescue,
     [:call, [:vcall, :p], :[], [:array, [:lit, 1]]], [:resbody,
     nil]]]],

  "p `echo \#$a \#@b \#@@c \#{$a+@b+@@c}`" =>
    [[:fcall, :p, [:array, [:dxstr, "echo ", [:evstr, [:gvar, :$a]
    ], [:str, " "], [:evstr, [:ivar, :@b]], [:str, " "], [:evstr, [
    :cvar, :@@c]], [:str, " "], [:evstr, [:call, [:call, [:gvar,
     :$a], :+, [:array, [:ivar, :@b]]], :+, [:array, [:cvar, :@@c]
    ]]]]]]],

  "p `echo \\\#$a \\\#@b \\\#@@c \\\#{$a+@b+@@c}`" =>
    [[:fcall, :p, [:array, [:xstr, "echo \#$a \#@b \#@@c \#{$a+@b+@@c}"]
    ]]],

  "p a rescue b" =>
    [[:rescue, [:fcall, :p, [:array, [:vcall, :a]]], [:resbody,
     nil, [:vcall, :b]]]],

  "p a,*b=c" =>
    [[:fcall, :p, [:argscat, [:array, [:vcall, :a]], [:lasgn, :b, [
    :vcall, :c]]]]],

  "p a,b,d=>b,*h do g end" =>
    [[:iter, [:fcall, :p, [:argscat, [:array, [:vcall, :a], [
    :vcall, :b], [:hash, [:vcall, :d], [:vcall, :b]]], [:vcall,
     :h]]], nil, [:vcall, :g]]],

  "p a,b,d=>b,*h,&g" =>
    [[:block_pass, [:vcall, :g], [:fcall, :p, [:argscat, [:array, [
    :vcall, :a], [:vcall, :b], [:hash, [:vcall, :d], [:vcall, :b]
    ]], [:vcall, :h]]]]],

  "p a0 = a rescue b,c" =>
    [[:fcall, :p, [:array, [:lasgn, :a0, [:rescue, [:vcall, :a],
     [:resbody, nil, [:vcall, :b]]]], [:vcall, :c]]]],

  "p a=b=1,d=2" =>
    [[:fcall, :p, [:array, [:lasgn, :a, [:lasgn, :b, [:lit, 1]]
    ], [:lasgn, :d, [:lit, 2]]]]],

  "p begin; a; rescue b; end" =>
    [[:fcall, :p, [:array, [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]]],

  "p begin; a; rescue b; end=>d" =>
    [[:fcall, :p, [:array, [:hash, [:rescue, [:vcall, :a], [
    :resbody, [:array, [:vcall, :b]]]], [:vcall, :d]]]]],

  "\
p class Pppp < String
   p=123
end\

" =>
    [[:fcall, :p, [:array, [:class, :Pppp, [:const, :String], [
    :scope, [:lasgn, :p, [:lit, 123]]]]]]],

  "p d=>b" =>
    [[:fcall, :p, [:array, [:hash, [:vcall, :d], [:vcall, :b]]]]
    ],

  "p d=>b do g end" =>
    [[:iter, [:fcall, :p, [:array, [:hash, [:vcall, :d], [:vcall,
     :b]]]], nil, [:vcall, :g]]],

  "p d=>b,&g" =>
    [[:block_pass, [:vcall, :g], [:fcall, :p, [:array, [:hash, [
    :vcall, :d], [:vcall, :b]]]]]],

  "p d=>b,*h" =>
    [[:fcall, :p, [:argscat, [:array, [:hash, [:vcall, :d], [
    :vcall, :b]]], [:vcall, :h]]]],

  "p d=>b,*h do g end" =>
    [[:iter, [:fcall, :p, [:argscat, [:array, [:hash, [:vcall, :d], [
    :vcall, :b]]], [:vcall, :h]]], nil, [:vcall, :g]]],

  "p d=>b,*h,&g" =>
    [[:block_pass, [:vcall, :g], [:fcall, :p, [:argscat, [:array, [
    :hash, [:vcall, :d], [:vcall, :b]]], [:vcall, :h]]]]],

  "p d=>begin; a; rescue b; end" =>
    [[:fcall, :p, [:array, [:hash, [:vcall, :d], [:rescue, [
    :vcall, :a], [:resbody, [:array, [:vcall, :b]]]]]]]],

  "\
p def pppp
   p=123
end\

" =>
    [[:fcall, :p, [:array, [:defn, :pppp, [:scope, [:block, [
    :args], [:lasgn, :p, [:lit, 123]]]]]]],
    {:warnings=>["(string):1: warning: void value expression"]}],

  "p else=5" => SyntaxError.new("(string):1: syntax error, unexpected kELSE, expecting $end\np else=5\n      ^"),
  "p end=5" => SyntaxError.new("(string):1: syntax error, unexpected kEND, expecting $end\np end=5\n     ^"),
  "p eval \"%\\0foo\\0\"" =>
    [[:fcall, :p, [:array, [:fcall, :eval, [:array, [
    :str, "%\x00foo\x00"]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p eval \"%\\nfoo\\n\"" =>
    [[:fcall, :p, [:array, [:fcall, :eval, [:array, [
    :str, "%\nfoo\n"]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p eval \"%\\r\\nfoo\\r\\n\"" =>
    [[:fcall, :p, [:array, [:fcall, :eval, [:array, [
    :str, "%\r\nfoo\r\n"]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p eval \"%\\rfoo\\r\"" =>
    [[:fcall, :p, [:array, [:fcall, :eval, [:array, [
    :str, "%\rfoo\r"]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p eval \"%\\sfoo\\s\"" =>
    [[:fcall, :p, [:array, [:fcall, :eval, [:array, [:str, "% foo "]
    ]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p eval \"%\\tfoo\\t\"" =>
    [[:fcall, :p, [:array, [:fcall, :eval, [:array, [
    :str, "%\tfoo\t"]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p eval \"%\\vfoo\\v\"" =>
    [[:fcall, :p, [:array, [:fcall, :eval, [:array, [
    :str, "%\vfoo\v"]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p foo" => [[:fcall, :p, [:array, [:vcall, :foo]]]],
  "p if=5" => SyntaxError.new("(string):1: syntax error, unexpected '='\np if=5\n     ^"),
  "p jd_to_wday(98)" =>
    [[:fcall, :p, [:array, [:fcall, :jd_to_wday, [:array, [:lit,
     98]]]]]],

  "\
p module Ppp
   p=123
end\

" =>
    [[:fcall, :p, [:array, [:module, :Ppp, [:scope, [:lasgn, :p,
     [:lit, 123]]]]]]],

  "p or=5" => SyntaxError.new("(string):1: syntax error, unexpected '='\np or=5\n     ^"),
  "p p ().m" =>
    [[:fcall, :p, [:array, [:call, [:fcall, :p], :m]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "p p (1).m" =>
    [[:fcall, :p, [:array, [:call, [:fcall, :p, [:array, [:lit,
     1]]], :m]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "p p (1,2).m" =>
    [[:fcall, :p, [:array, [:call, [:fcall, :p, [:array, [:lit,
     1], [:lit, 2]]], :m]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "p p :c, :d" =>
    [[:fcall, :p, [:array, [:fcall, :p, [:array, [:lit, :c], [
    :lit, :d]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p p {} do end" =>
    [[:iter, [:fcall, :p, [:array, [:iter, [:fcall, :p], nil]]],
     nil]],

  "p p(1).m" =>
    [[:fcall, :p, [:array, [:call, [:fcall, :p, [:array, [:lit,
     1]]], :m]]]],

  "p p:'b'" =>
    [[:fcall, :p, [:array, [:fcall, :p, [:array, [:lit, :b]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p pppp" => [[:fcall, :p, [:array, [:vcall, :pppp]]]],
  "\
p proc {
   p=123
}.call\

" =>
    [[:fcall, :p, [:array, [:call, [:iter, [:fcall, :proc], nil,
     [:dasgn_curr, :p, [:lit, 123]]], :call]]]],

  "\
p proc{|
a,b,(c,d,),e|
        p a %(1)
        p b %(2)
        p c %(3)
        p d %(4)
        p e %(5)
[ a,b,c,d,e]}.call(1,2,[3,4],5)\

" =>
    [[:fcall, :p, [:array, [:call, [:iter, [:fcall, :proc], [
    :masgn, [:array, [:dasgn_curr, :a], [:dasgn_curr, :b], [
    :masgn, [:array, [:dasgn_curr, :c], [:dasgn_curr, :d]], nil,
     nil], [:dasgn_curr, :e]], nil, nil], [:block, [:fcall, :p, [
    :array, [:call, [:dvar, :a], :%, [:array, [:lit, 1]]]]], [
    :fcall, :p, [:array, [:call, [:dvar, :b], :%, [:array, [:lit,
     2]]]]], [:fcall, :p, [:array, [:call, [:dvar, :c], :%, [
    :array, [:lit, 3]]]]], [:fcall, :p, [:array, [:call, [:dvar,
     :d], :%, [:array, [:lit, 4]]]]], [:fcall, :p, [:array, [
    :call, [:dvar, :e], :%, [:array, [:lit, 5]]]]], [:array, [
    :dvar, :a], [:dvar, :b], [:dvar, :c], [:dvar, :d], [:dvar, :e]
    ]]], :call, [:array, [:lit, 1], [:lit, 2], [:array, [:lit, 3], [
    :lit, 4]], [:lit, 5]]]]]],

  "p proc{||}" =>
    [[:fcall, :p, [:array, [:iter, [:fcall, :proc], 0]]]],

  "p simple" => [[:fcall, :p, [:array, [:vcall, :simple]]]],
  "p simple\r" => [[:fcall, :p, [:array, [:vcall, :simple]]]],
  "p timeout(5) {45}" =>
    [[:fcall, :p, [:array, [:iter, [:fcall, :timeout, [:array, [
    :lit, 5]]], nil, [:lit, 45]]]]],

  "\
p true ?
  1 : 2\

" =>
    [[:fcall, :p, [:array, [:if, [:true], [:lit, 1], [:lit, 2]]]
    ]],

  "p {}" => [[:iter, [:fcall, :p], nil]],
  "p( %r{\\/$})" => [[:fcall, :p, [:array, [:lit, /\/$/]]]],
  "p( %r~<!include:([\\/\\w\\.\\-]+)>~m)" =>
    [[:fcall, :p, [:array, [:lit, /<!include:([\/\w\.\-]+)>/m]]]
    ],

  "\
p( <<end )
nine time nine men have stood untold.
end\

" =>
    [[:fcall, :p, [:array, [:str, "nine time nine men have stood untold.\n"]
    ]]],

  "p( p + 5 )" =>
    [[:fcall, :p, [:array, [:call, [:vcall, :p], :+, [:array, [
    :lit, 5]]]]]],

  "p( p +5 )" =>
    [[:fcall, :p, [:array, [:fcall, :p, [:array, [:lit, 5]]]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces", "(string):1: warning: parenthesize argument(s) for future version"]}],

  "p( p [ ] )" =>
    [[:fcall, :p, [:array, [:fcall, :p, [:array, [:zarray]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p( p [] )" =>
    [[:fcall, :p, [:array, [:fcall, :p, [:array, [:zarray]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p( p+5 )" =>
    [[:fcall, :p, [:array, [:call, [:vcall, :p], :+, [:array, [
    :lit, 5]]]]]],

  "p( p[] )" =>
    [[:fcall, :p, [:array, [:call, [:vcall, :p], :[]]]]],

  "p( {:rest=>99})" =>
    [[:fcall, :p, [:array, [:hash, [:lit, :rest], [:lit, 99]]]]],

  "p(\"\\\\\")" => [[:fcall, :p, [:array, [:str, "\\"]]]],
  "p(% foo )" => [[:fcall, :p, [:array, [:str, "foo"]]]],
  "p(%r[foo]i)" => [[:fcall, :p, [:array, [:lit, /foo/i]]]],
  "p(&a.b)" =>
    [[:block_pass, [:call, [:vcall, :a], :b], [:fcall, :p]]],

  "p(&begin; a; rescue b; end)" =>
    [[:block_pass, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]], [:fcall, :p]]],

  "p((((*a=c))))" =>
    [[:fcall, :p, [:array, [:masgn, nil, [:lasgn, :a], [:array, [
    :vcall, :c]]]]]],

  "p(((*a=c)))" =>
    [[:fcall, :p, [:array, [:masgn, nil, [:lasgn, :a], [:array, [
    :vcall, :c]]]]]],

  "p((*a=c))" =>
    [[:fcall, :p, [:array, [:masgn, nil, [:lasgn, :a], [:array, [
    :vcall, :c]]]]]],

  "p()" => [[:fcall, :p]],
  "p().method(:each)" =>
    [[:call, [:fcall, :p], :method, [:array, [:lit, :each]]]],

  "p(){|a=b=1,d=2| e}" => SyntaxError.new("(string):1: syntax error, unexpected '=', expecting tCOLON2 or '[' or '.'\np(){|a=b=1,d=2| e}\n       ^\n(string):1: syntax error, unexpected '}', expecting $end\np(){|a=b=1,d=2| e}\n                  ^"),
  "p(*a+b)" =>
    [[:fcall, :p, [:splat, [:call, [:vcall, :a], :+, [:array, [
    :vcall, :b]]]]]],

  "p(*a=c)" =>
    [[:fcall, :p, [:splat, [:lasgn, :a, [:vcall, :c]]]]],

  "p(*b=c)" =>
    [[:fcall, :p, [:splat, [:lasgn, :b, [:vcall, :c]]]]],

  "p(*begin; a; rescue b; end)" =>
    [[:fcall, :p, [:splat, [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]]],

  "p(-1)" => [[:fcall, :p, [:array, [:lit, -1]]]],
  "p(/\#$a \#@b \#@@c \#{$a+@b+@@c}/)" =>
    [[:fcall, :p, [:array, [:dregx, "", [:evstr, [:gvar, :$a]], [
    :str, " "], [:evstr, [:ivar, :@b]], [:str, " "], [:evstr, [
    :cvar, :@@c]], [:str, " "], [:evstr, [:call, [:call, [:gvar,
     :$a], :+, [:array, [:ivar, :@b]]], :+, [:array, [:cvar, :@@c]
    ]]]]]]],

  "p(/[\\\\]/)" => [[:fcall, :p, [:array, [:lit, /[\\]/]]]],
  "\
p(/
/)\

" =>
    [[:fcall, :p, [:array, [:lit, /
/]]]],

  "p(/^\\s*(([+-\\/*&\\|^]|<<|>>|\\|\\||\\&\\&)=|\\&\\&|\\|\\|)\
/)" =>
    [[:fcall, :p, [:array, [
    :lit, /^\s*(([+-\/*&\|^]|<<|>>|\|\||\&\&)=|\&\&|\|\|)/]]]],

  "p(/\\\\/)" => [[:fcall, :p, [:array, [:lit, /\\/]]]],
  "p(/\\n/)" => [[:fcall, :p, [:array, [:lit, /\n/]]]],
  "p(1,2)     " => [[:fcall, :p, [:array, [:lit, 1], [:lit, 2]]]],
  "p(1-2).nil?     " =>
    [[:call, [:fcall, :p, [:array, [:call, [:lit, 1], :-, [:array, [
    :lit, 2]]]]], :nil?]],

  "p(1.+1)" =>
    [[:fcall, :p, [:array, [:call, [:lit, 1], :+, [:array, [:lit,
     1]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p(1..10).method(:each)" =>
    [[:call, [:fcall, :p, [:array, [:lit, 1..10]]], :method, [
    :array, [:lit, :each]]]],

  "p(1..10,1).method(:each)" =>
    [[:call, [:fcall, :p, [:array, [:lit, 1..10], [:lit, 1]]],
     :method, [:array, [:lit, :each]]]],

  "p(1.10).me,z=c" =>
    [[:masgn, [:array, [:attrasgn, [:fcall, :p, [:array, [:lit,
     1.1]]], :me=], [:lasgn, :z]], nil, [:to_ary, [:vcall, :c]]]
    ],

  "p(:\"[]\")" => [[:fcall, :p, [:array, [:lit, :[]]]]],
  "p(:%)" => [[:fcall, :p, [:array, [:lit, :%]]]],
  "p(:f){}" => [[:iter, [:fcall, :p, [:array, [:lit, :f]]], nil]],
  "p(:p)" => [[:fcall, :p, [:array, [:lit, :p]]]],
  "p(:side=>:top)" =>
    [[:fcall, :p, [:array, [:hash, [:lit, :side], [:lit, :top]]]
    ]],

  "\
p(<<-jjj +
 dsfgdf
 jjj
 \"dfsdfs\"
 )\

" =>
    [[:fcall, :p, [:array, [:call, [:str, " dsfgdf\n"], :+, [
    :array, [:str, "dfsdfs"]]]]]],

  "\
p(<<-jjj \\
 dsfgdf
 jjj
 )\

" =>
    [[:fcall, :p, [:array, [:str, " dsfgdf\n"]]]],

  "\
p(<<-jjj \\
 dsfgdf
 jjj
 +\"dfsdfs\"
 )\

" =>
    [[:fcall, :p, [:array, [:call, [:str, " dsfgdf\n"], :+, [
    :array, [:str, "dfsdfs"]]]]]],

  "\
p(<<-jjj
 dsfgdf
 jjj
 )\

" =>
    [[:fcall, :p, [:array, [:str, " dsfgdf\n"]]]],

  "\
p(<<stuff+'foobar')\\
+\"more stuff\"
some stuff
stuff
\np <<stuff+'foobar'\\
+\"more stuff\"
some stuff
stuff
\np <<stuff+'foobar'+
some stuff
stuff
\"more stuff\"
\np <<stuff+'foobar'+<<baggage+\"baz\"+<<impedimentia+
some stuff
stuff
blah blah
baggage
hic haec hoc
impedimentia
\"more stuff\"
\n\

" =>
    [[:block, [:fcall, :p, [:array, [:call, [
    :str, "+\"more stuff\"\nsome stuff\n"], :+, [:array, [:str,
     "foobar"]]]]], [:fcall, :p, [:array, [:call, [
    :str, "+\"more stuff\"\nsome stuff\n"], :+, [:array, [:str,
     "foobar"]]]]], [:fcall, :p, [:array, [:call, [:call, [:str,
     "some stuff\n"], :+, [:array, [:str, "foobar"]]], :+, [
    :array, [:str, "more stuff"]]]]], [:fcall, :p, [:array, [
    :call, [:call, [:call, [:call, [:call, [:str, "some stuff\n"],
     :+, [:array, [:str, "foobar"]]], :+, [:array, [
    :str, "blah blah\n"]]], :+, [:array, [:str, "baz"]]], :+, [
    :array, [:str, "hic haec hoc\n"]]], :+, [:array, [
    :str, "more stuff"]]]]]]],

  "p(Module.instance_methods - Object.instance_methods).sort \
 " =>
    [[:call, [:fcall, :p, [:array, [:call, [:call, [
    :const, :Module], :instance_methods], :-, [:array, [:call, [
    :const, :Object], :instance_methods]]]]], :sort]],

  "p(P? ? 1 : 2)" =>
    [[:fcall, :p, [:array, [:if, [:fcall, :P?], [:lit, 1], [:lit,
     2]]]]],

  "p(P?? 1 : 2)" =>
    [[:fcall, :p, [:array, [:if, [:fcall, :P?], [:lit, 1], [:lit,
     2]]]]],

  "p(String * Class)" =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :*, [:array, [
    :const, :Class]]]]]],

  "p(String *Class)" =>
    [[:fcall, :p, [:array, [:fcall, :String, [:splat, [:const,
     :Class]]]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix", "(string):1: warning: parenthesize argument(s) for future version"]}],

  "p(String / Class) " =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :/, [:array, [
    :const, :Class]]]]]],

  "p(String /Class/)" =>
    [[:fcall, :p, [:array, [:fcall, :String, [:array, [
    :lit, /Class/]]]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces", "(string):1: warning: parenthesize argument(s) for future version"]}],

  "p(String :: Class)" =>
    [[:fcall, :p, [:array, [:fcall, :String, [:array, [:colon3,
     :Class]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p(String ::Class)" =>
    [[:fcall, :p, [:array, [:fcall, :String, [:array, [:colon3,
     :Class]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "\
p(String <<Class)
sgsdfgf
  Class
Class\

" =>
    [[:fcall, :p, [:array, [:fcall, :String, [:array, [
    :str, "sgsdfgf\n  Class\n"]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p(String [ Class])" =>
    [[:fcall, :p, [:array, [:fcall, :String, [:array, [:array, [
    :const, :Class]]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p(String [Class])" =>
    [[:fcall, :p, [:array, [:fcall, :String, [:array, [:array, [
    :const, :Class]]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p(String* Class)" =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :*, [:array, [
    :const, :Class]]]]]],

  "p(String*Class)" =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :*, [:array, [
    :const, :Class]]]]]],

  "p(String/ Class)" =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :/, [:array, [
    :const, :Class]]]]]],

  "p(String/Class)" =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :/, [:array, [
    :const, :Class]]]]]],

  "p(String:: Class)" =>
    [[:fcall, :p, [:array, [:colon2, [:const, :String], :Class]]
    ]],

  "p(String::Class)" =>
    [[:fcall, :p, [:array, [:colon2, [:const, :String], :Class]]
    ]],

  "\
p(String<<- Class)
p(String<< - Class)
p(String <<- Class)
  Class
Class
p(String <<- Class)
Class
  Class
Class
p(String << - Class)\

" =>
    [[:block, [:fcall, :p, [:array, [:call, [:const, :String], :<<, [
    :array, [:call, [:const, :Class], :-@]]]]], [:fcall, :p, [
    :array, [:call, [:const, :String], :<<, [:array, [:call, [
    :const, :Class], :-@]]]]], [:fcall, :p, [:array, [:call, [
    :const, :String], :<<, [:array, [:call, [:const, :Class], :-@]
    ]]]], [:const, :Class], [:const, :Class], [:fcall, :p, [
    :array, [:call, [:const, :String], :<<, [:array, [:call, [
    :const, :Class], :-@]]]]], [:const, :Class], [:const, :Class], [
    :const, :Class], [:fcall, :p, [:array, [:call, [
    :const, :String], :<<, [:array, [:call, [:const, :Class], :-@]
    ]]]]],
    {:warnings=>["(string):4: warning: useless use of a constant in void context", "(string):5: warning: useless use of a constant in void context", "(string):7: warning: useless use of a constant in void context", "(string):8: warning: useless use of a constant in void context", "(string):9: warning: useless use of a constant in void context"]}],

  "\
p(String<<-Class)
p(String<< -Class)
p(String <<-Class)
sgsdfgf
  Class
Class
p(String <<-Class)
sgsdfgf
Class
  Class
Class
p(String << -Class)\

" =>
    [[:block, [:fcall, :p, [:array, [:call, [:const, :String], :<<, [
    :array, [:call, [:const, :Class], :-@]]]]], [:fcall, :p, [
    :array, [:call, [:const, :String], :<<, [:array, [:call, [
    :const, :Class], :-@]]]]], [:fcall, :p, [:array, [:fcall, :String, [
    :array, [:str, "sgsdfgf\n"]]]]], [:const, :Class], [:fcall,
     :p, [:array, [:fcall, :String, [:array, [:str, "sgsdfgf\n"]
    ]]]], [:const, :Class], [:const, :Class], [:fcall, :p, [
    :array, [:call, [:const, :String], :<<, [:array, [:call, [
    :const, :Class], :-@]]]]]],
    {:warnings=>["(string):3: warning: parenthesize argument(s) for future version", "(string):7: warning: parenthesize argument(s) for future version", "(string):6: warning: useless use of a constant in void context", "(string):10: warning: useless use of a constant in void context", "(string):11: warning: useless use of a constant in void context"]}],

  "\
p(String<<Class)
p(String<< Class)
p(String <<Class)
sgsdfgf
  Class
Class
p(String << Class)\

" =>
    [[:block, [:fcall, :p, [:array, [:call, [:const, :String], :<<, [
    :array, [:const, :Class]]]]], [:fcall, :p, [:array, [:call, [
    :const, :String], :<<, [:array, [:const, :Class]]]]], [
    :fcall, :p, [:array, [:fcall, :String, [:array, [
    :str, "sgsdfgf\n  Class\n"]]]]], [:fcall, :p, [:array, [
    :call, [:const, :String], :<<, [:array, [:const, :Class]]]]]
    ],
    {:warnings=>["(string):3: warning: parenthesize argument(s) for future version"]}],

  "p(String[ Class])" =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :[], [
    :array, [:const, :Class]]]]]],

  "p(String[Class])" =>
    [[:fcall, :p, [:array, [:call, [:const, :String], :[], [
    :array, [:const, :Class]]]]]],

  "p(a and b)" => SyntaxError.new("(string):1: syntax error, unexpected kAND, expecting ')'\np(a and b)\n       ^"),
  "p(a,*b=c)" =>
    [[:fcall, :p, [:argscat, [:array, [:vcall, :a]], [:lasgn, :b, [
    :vcall, :c]]]]],

  "p(a,b,d=>b,*h){g}" =>
    [[:iter, [:fcall, :p, [:argscat, [:array, [:vcall, :a], [
    :vcall, :b], [:hash, [:vcall, :d], [:vcall, :b]]], [:vcall,
     :h]]], nil, [:vcall, :g]]],

  "p(a0 = a rescue b,c)" =>
    [[:fcall, :p, [:array, [:lasgn, :a0, [:rescue, [:vcall, :a],
     [:resbody, nil, [:vcall, :b]]]], [:vcall, :c]]]],

  "p(a=b=1,d=2)" =>
    [[:fcall, :p, [:array, [:lasgn, :a, [:lasgn, :b, [:lit, 1]]
    ], [:lasgn, :d, [:lit, 2]]]]],

  "p(begin; a; rescue b; end)" =>
    [[:fcall, :p, [:array, [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]]],

  "p(d=>b){g}" =>
    [[:iter, [:fcall, :p, [:array, [:hash, [:vcall, :d], [:vcall,
     :b]]]], nil, [:vcall, :g]]],

  "p(d=>b,*h){g}" =>
    [[:iter, [:fcall, :p, [:argscat, [:array, [:hash, [:vcall, :d], [
    :vcall, :b]]], [:vcall, :h]]], nil, [:vcall, :g]]],

  "p(false :: to_s)" =>
    [[:fcall, :p, [:array, [:call, [:false], :to_s]]]],

  "p(false ::to_s)" =>
    [[:fcall, :p, [:array, [:call, [:false], :to_s]]]],

  "p(false:: to_s)" =>
    [[:fcall, :p, [:array, [:call, [:false], :to_s]]]],

  "p(false::to_s)" =>
    [[:fcall, :p, [:array, [:call, [:false], :to_s]]]],

  "p(p (1).m)" =>
    [[:fcall, :p, [:array, [:fcall, :p, [:array, [:call, [:lit,
     1], :m]]]]],
    {:warnings=>["(string):1: warning: (...) interpreted as grouped expression", "(string):1: warning: parenthesize argument(s) for future version"]}],

  "p(p ^6)" =>
    [[:fcall, :p, [:array, [:call, [:vcall, :p], :^, [:array, [
    :lit, 6]]]]]],

  "p(p? ? 1 : 2)" =>
    [[:fcall, :p, [:array, [:if, [:fcall, :p?], [:lit, 1], [:lit,
     2]]]]],

  "p(p?? 1 : 2)" =>
    [[:fcall, :p, [:array, [:if, [:fcall, :p?], [:lit, 1], [:lit,
     2]]]]],

  "\
p(proc do
   p=123
end.call)\

" =>
    [[:fcall, :p, [:array, [:call, [:iter, [:fcall, :proc], nil,
     [:dasgn_curr, :p, [:lit, 123]]], :call]]]],

  "p({1=>2,3=>4,})" =>
    [[:fcall, :p, [:array, [:hash, [:lit, 1], [:lit, 2], [:lit,
     3], [:lit, 4]]]]],

  "p({1=>2,3=>4}) { foo bar baz }" =>
    [[:iter, [:fcall, :p, [:array, [:hash, [:lit, 1], [:lit, 2],
     [:lit, 3], [:lit, 4]]]], nil, [:fcall, :foo, [:array, [
    :fcall, :bar, [:array, [:vcall, :baz]]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version"]}],

  "p({:foo=>:bar})   " =>
    [[:fcall, :p, [:array, [:hash, [:lit, :foo], [:lit, :bar]]]]
    ],

  "p(~4){}" =>
    [[:iter, [:fcall, :p, [:array, [:call, [:lit, 4], :~]]], nil]
    ],

  "p:'b'" => [[:fcall, :p, [:array, [:lit, :b]]]],
  "p:f" => [[:fcall, :p, [:array, [:lit, :f]]]],
  "p:p8" => [[:fcall, :p, [:array, [:lit, :p8]]]],
  "p=556;p (e) /a" =>
    [[:block, [:lasgn, :p, [:lit, 556]], [:call, [:fcall, :p, [
    :array, [:vcall, :e]]], :/, [:array, [:vcall, :a]]]]],

  "p? :p8" => [[:fcall, :p?, [:array, [:lit, :p8]]]],
  "p? ?1" => [[:fcall, :p?, [:array, [:lit, 49]]]],
  "p?:p8" => [[:fcall, :p?, [:array, [:lit, :p8]]]],
  "p??1" => [[:fcall, :p?, [:array, [:lit, 49]]]],
  "p??y" => [[:fcall, :p?, [:array, [:lit, 121]]]],
  "p\r44" => [[:fcall, :p, [:array, [:lit, 44]]]],
  "print \"coled: \" + $! +\" wiin \#{@ray}\\n\";" =>
    [[:fcall, :print, [:array, [:call, [:call, [:str, "coled: "],
     :+, [:array, [:gvar, :$!]]], :+, [:array, [:dstr, " wiin ",
     [:evstr, [:ivar, :@ray]], [:str, "\n"]]]]]]],

  "proc{ BEGIN{a=1};a }" =>
    [[:iter, [:fcall, :proc], nil, [:vcall, :a]]],

  "proc{ END{a=1};a }" =>
    [[:iter, [:fcall, :proc], nil, [:block, [:iter, [:postexe],
     nil, [:dasgn_curr, :a, [:lit, 1]]], [:dvar, :a]]]],

  "proc{ proc{ BEGIN{a=1};a } }" =>
    [[:iter, [:fcall, :proc], nil, [:iter, [:fcall, :proc], nil,
     [:vcall, :a]]]],

  "proc{ proc{ END{a=1};a } }" =>
    [[:iter, [:fcall, :proc], nil, [:iter, [:fcall, :proc], nil,
     [:block, [:iter, [:postexe], nil, [:dasgn_curr, :a, [:lit,
     1]]], [:dvar, :a]]]]],

  "\
proc{
p %=b=
2
p %(1)
}\

" =>
    [[:iter, [:fcall, :proc], nil, [:block, [:dasgn_curr, :p, [
    :call, [:dvar, :p], :%, [:array, [:dasgn_curr, :b, [:lit, 2]
    ]]]], [:call, [:dvar, :p], :%, [:array, [:lit, 1]]]]]],

  "\
proc{
p &nil
p &=1
p %(1)
}\

" =>
    [[:iter, [:fcall, :proc], nil, [:block, [:block_pass, [:nil], [
    :fcall, :p]], [:dasgn_curr, :p, [:call, [:dvar, :p], :&, [
    :array, [:lit, 1]]]], [:call, [:dvar, :p], :%, [:array, [
    :lit, 1]]]]],
    {:warnings=>["(string):2: warning: `&' interpreted as argument prefix"]}],

  "\
proc{
p *=5
p %(1)
}\

" =>
    [[:iter, [:fcall, :proc], nil, [:block, [:dasgn_curr, :p, [
    :call, [:dvar, :p], :*, [:array, [:lit, 5]]]], [:call, [
    :dvar, :p], :%, [:array, [:lit, 1]]]]]],

  "\
proc{
p /$/  #regexp
p /=$/ #operator
p /$/  #operator
}\

" =>
    [[:iter, [:fcall, :proc], nil, [:block, [:fcall, :p, [:array, [
    :lit, /$/]]], [:dasgn_curr, :p, [:call, [:dvar, :p], :/, [
    :array, [:gvar, :$/]]]], [:call, [:dvar, :p], :/, [:array, [
    :gvar, :$/]]]]],
    {:warnings=>["(string):2: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "\
proc{q=1;def q.foo; end}  #q should be varnametoken, both times
module Defined_p_syntax_tests
  def self.defined?(foo) :baz end  #should be methname
  def defined?(foo) :bar end  #should be methname
  def ameth
    p(defined? 44)  #should be keyword
    p(self.defined? 44) #should be methname
  end
end" =>
    [[:block, [:iter, [:fcall, :proc], nil, [:block, [:dasgn_curr,
     :q, [:lit, 1]], [:defs, [:dvar, :q], :foo, [:scope, [:args]
    ]]]], [:module, :Defined_p_syntax_tests, [:scope, [:block, [
    :defs, [:self], :defined?, [:scope, [:block, [:args, :foo], [
    :lit, :baz]]]], [:defn, :defined?, [:scope, [:block, [:args,
     :foo], [:lit, :bar]]]], [:defn, :ameth, [:scope, [:block, [
    :args], [:fcall, :p, [:array, [:defined, [:lit, 44]]]], [
    :fcall, :p, [:array, [:call, [:self], :defined?, [:array, [
    :lit, 44]]]]]]]]]]]],
    {:warnings=>["(string):7: warning: parenthesize argument(s) for future version"]}],

  "proc{yeild}" =>
    [[:iter, [:fcall, :proc], nil, [:vcall, :yeild]]],

  "\
proc{|(a,b)|}
proc{|a,b=10|
\n
}
proc{|a,b=\"#foobar\"|
\n}
proc{|a,b=(1|2)|
\n}
proc{|a,b=ccc=d=0|
  ccc %(1)
}
def ccc
\nend
proc{|a,b=ccc()|
  ccc %(1)
}
\ndef x(a,b=10)
end\

" =>
    SyntaxError.new("(string):2: syntax error, unexpected '=', expecting '|'\nproc{|a,b=10|\n          ^\n(string):6: syntax error, unexpected '=', expecting '|'\nproc{|a,b=\"#foobar\"|\n          ^\n(string):8: syntax error, unexpected '}'\n(string):9: syntax error, unexpected '=', expecting '|'\nproc{|a,b=(1|2)|\n          ^\n(string):11: syntax error, unexpected '}'\n(string):12: syntax error, unexpected '=', expecting '|'\nproc{|a,b=ccc=d=0|\n          ^\n(string):14: syntax error, unexpected '}', expecting $end"),

  "proc{|(m).kk,(m+n).kk| }" =>
    [[:iter, [:fcall, :proc], [:masgn, [:array, [:attrasgn, [
    :vcall, :m], :kk=], [:attrasgn, [:call, [:vcall, :m], :+, [
    :array, [:vcall, :n]]], :kk=]], nil, nil]]],

  "proc{|a,b,c,| p a,b,c }.call(1,2,3)" =>
    [[:call, [:iter, [:fcall, :proc], [:masgn, [:array, [
    :dasgn_curr, :a], [:dasgn_curr, :b], [:dasgn_curr, :c]], nil,
     nil], [:fcall, :p, [:array, [:dvar, :a], [:dvar, :b], [
    :dvar, :c]]]], :call, [:array, [:lit, 1], [:lit, 2], [:lit,
     3]]]],

  "proc{|a.b,c.d|}" =>
    [[:iter, [:fcall, :proc], [:masgn, [:array, [:attrasgn, [
    :vcall, :a], :b=], [:attrasgn, [:vcall, :c], :d=]], nil, nil]
    ]],

  "\
public
def printer; puts self end
alias+@printer
\n  0<2_500_000and+\"yippeee\"\

" =>
    [[:block, [:vcall, :public], [:defn, :printer, [:scope, [
    :block, [:args], [:fcall, :puts, [:array, [:self]]]]]], [
    :alias, [:lit, :+@], [:lit, :printer]], [:and, [:call, [:lit,
     0], :<, [:array, [:lit, 2500000]]], [:call, [:str, "yippeee"],
     :+@]]]],

  "\
puts \"%\\sfoo\\s \"
\nputs \"%\\tfoo\\t \"
puts \"%\\vfoo\\v \"
puts \"%\\rfoo\\r \"
puts \"%\\nfoo\\n \"
puts \"%\\0foo\\0 \"
\nputs \"%\\r\\nfoo\\r\\n \"
\n#given that \\n\\r is not considered a valid eol
#sequence in ruby (unlike \\r\\n), the next 2 are ok
puts \"%\\n\\rfoo\\n\\r \" 
puts \"%\\n\\rfoo\\n \"                                 
\n#seems like just \\r isn't valid newline either, so
#the following are are correct.
puts \"%\\r\\nfoo\\n \" 
puts \"%\\rfoo\\r \"  
puts \"%\\nfoo\\n \"    
puts \"%\\nfoo\\r\\n \"    
\n" =>
    [[:block, [:fcall, :puts, [:array, [:str, "% foo  "]]], [
    :fcall, :puts, [:array, [:str, "%\tfoo\t "]]], [:fcall, :puts, [
    :array, [:str, "%\vfoo\v "]]], [:fcall, :puts, [:array, [
    :str, "%\rfoo\r "]]], [:fcall, :puts, [:array, [:str, "%\nfoo\n "]]], [
    :fcall, :puts, [:array, [:str, "%\x00foo\x00 "]]], [:fcall,
     :puts, [:array, [:str, "%\r\nfoo\r\n "]]], [:fcall, :puts, [
    :array, [:str, "%\n\rfoo\n\r "]]], [:fcall, :puts, [:array, [
    :str, "%\n\rfoo\n "]]], [:fcall, :puts, [:array, [
    :str, "%\r\nfoo\n "]]], [:fcall, :puts, [:array, [
    :str, "%\rfoo\r "]]], [:fcall, :puts, [:array, [:str, "%\nfoo\n "]]], [
    :fcall, :puts, [:array, [:str, "%\nfoo\r\n "]]]]],

  "p{}" => [[:iter, [:fcall, :p], nil]],
  "p~4" => [[:fcall, :p, [:array, [:call, [:lit, 4], :~]]]],
  "r (a,b=c,d)" =>
    [[:fcall, :r, [:array, [:vcall, :a], [:lasgn, :b, [:vcall, :c]
    ], [:vcall, :d]]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "r (a,b=c,d)|1" => SyntaxError.new("(string):1: syntax error, unexpected '|', expecting $end\nr (a,b=c,d)|1\n            ^"),
  "raise" => [[:vcall, :raise]],
  "raise \"foo\"" => [[:fcall, :raise, [:array, [:str, "foo"]]]],
  "raise * @com_disk" =>
    [[:call, [:vcall, :raise], :*, [:array, [:ivar, :@com_disk]]
    ]],

  "raise - @com_disk" =>
    [[:call, [:vcall, :raise], :-, [:array, [:ivar, :@com_disk]]
    ]],

  "raise RuntimeError" =>
    [[:fcall, :raise, [:array, [:const, :RuntimeError]]]],

  "raise RuntimeError,\"foo\"" =>
    [[:fcall, :raise, [:array, [:const, :RuntimeError], [:str,
     "foo"]]]],

  "raise RuntimeError.new(\"foo\")" =>
    [[:fcall, :raise, [:array, [:call, [:const, :RuntimeError],
     :new, [:array, [:str, "foo"]]]]]],

  "require 'foo'" => [[:fcall, :require, [:array, [:str, "foo"]]]],
  "return" => [[:return]],
  "return % 1 " => [[:return, [:str, "1"]]],
  "return (@images = @old_imgs)" =>
    [[:return, [:iasgn, :@images, [:ivar, :@old_imgs]]]],

  "return (a,b=c,d)" =>
    [[:return, [:masgn, [:array, [:lasgn, :a], [:lasgn, :b]], nil, [
    :array, [:vcall, :c], [:vcall, :d]]]]],

  "return (a,b=c,d)|1" =>
    [[:return, [:call, [:masgn, [:array, [:lasgn, :a], [:lasgn,
     :b]], nil, [:array, [:vcall, :c], [:vcall, :d]]], :|, [
    :array, [:lit, 1]]]]],

  "return * @com_disk" =>
    [[:return, [:svalue, [:splat, [:ivar, :@com_disk]]]]],

  "return *a " => [[:return, [:svalue, [:splat, [:vcall, :a]]]]],
  "return - @com_disk" =>
    [[:return, [:call, [:ivar, :@com_disk], :-@]]],

  "return / 1 /" => [[:return, [:lit, / 1 /]]],
  "return 1" => [[:return, [:lit, 1]]],
  "return 1,*b" =>
    [[:return, [:argscat, [:array, [:lit, 1]], [:vcall, :b]]]],

  "return 1,2" => [[:return, [:array, [:lit, 1], [:lit, 2]]]],
  "return 1,2,*c" =>
    [[:return, [:argscat, [:array, [:lit, 1], [:lit, 2]], [
    :vcall, :c]]]],

  "return 1,2,3" =>
    [[:return, [:array, [:lit, 1], [:lit, 2], [:lit, 3]]]],

  "return ::A" => [[:return, [:colon3, :A]]],
  "return ENV[]" => [[:return, [:call, [:const, :ENV], :[]]]],
  "return a" => [[:return, [:vcall, :a]]],
  "return a rescue b" =>
    [[:rescue, [:return, [:vcall, :a]], [:resbody, nil, [:vcall,
     :b]]]],

  "return a,b" =>
    [[:return, [:array, [:vcall, :a], [:vcall, :b]]]],

  "return a,b,*c" =>
    [[:return, [:argscat, [:array, [:vcall, :a], [:vcall, :b]], [
    :vcall, :c]]]],

  "return a,b,*c do d end" =>
    [[:return, [:argscat, [:array, [:vcall, :a], [:vcall, :b]], [
    :iter, [:fcall, :c], nil, [:vcall, :d]]]]],

  "return rval / precision" =>
    [[:return, [:call, [:vcall, :rval], :/, [:array, [:vcall,
     :precision]]]]],

  "return ~1" => [[:return, [:call, [:lit, 1], :~]]],
  "return()" => [[:return, [:nil]]],
  "return(1)" => [[:return, [:lit, 1]]],
  "return(a and b)" =>
    [[:return, [:and, [:vcall, :a], [:vcall, :b]]]],

  "return[1]" => [[:return, [:array, [:lit, 1]]]],
  "s,((m).kk,(m+n).kk),t=3" =>
    [[:masgn, [:array, [:lasgn, :s], [:masgn, [:array, [:attrasgn, [
    :vcall, :m], :kk=], [:attrasgn, [:call, [:vcall, :m], :+, [
    :array, [:vcall, :n]]], :kk=]], nil, nil], [:lasgn, :t]], nil, [
    :to_ary, [:lit, 3]]]],

  "s,((m).kk,(m+n).kk)=3" =>
    [[:masgn, [:array, [:lasgn, :s], [:masgn, [:array, [:attrasgn, [
    :vcall, :m], :kk=], [:attrasgn, [:call, [:vcall, :m], :+, [
    :array, [:vcall, :n]]], :kk=]], nil, nil]], nil, [:to_ary, [
    :lit, 3]]]],

  "s=3; s::T" =>
    [[:block, [:lasgn, :s, [:lit, 3]], [:colon2, [:lvar, :s], :T]
    ]],

  "self" => [[:self]],
  "self <=> :p8" =>
    [[:call, [:self], :<=>, [:array, [:lit, :p8]]]],

  "self <=>:p8" => [[:call, [:self], :<=>, [:array, [:lit, :p8]]]],
  "self.<=> :p8" =>
    [[:call, [:self], :<=>, [:array, [:lit, :p8]]]],

  "self.<=>:p8" => [[:call, [:self], :<=>, [:array, [:lit, :p8]]]],
  "self.[] :p8" => [[:call, [:self], :[], [:array, [:lit, :p8]]]],
  "self.[]:p8" => [[:call, [:self], :[], [:array, [:lit, :p8]]]],
  "self;a" =>
    [[:block, [:self], [:vcall, :a]],
    {:warnings=>["(string):1: warning: useless use of self in void context"]}],

  "self[-1] += succ_table" =>
    [[:op_asgn1, [:self], [:array, [:lit, -1]], :+, [
    :vcall, :succ_table]]],

  "self[]" => [[:fcall, :[]]],
  "self[]=b" =>
    [[:attrasgn, [:self], :[]=, [:array, [:vcall, :b]]]],

  "self[a]" => [[:fcall, :[], [:array, [:vcall, :a]]]],
  "self[a]=b" =>
    [[:attrasgn, [:self], :[]=, [:array, [:vcall, :a], [:vcall,
     :b]]]],

  "sn = m.sn %(sen)" =>
    [[:lasgn, :sn, [:call, [:vcall, :m], :sn, [:array, [
    :str, "sen"]]]]],

  "sn = m.sn /s/" =>
    [[:lasgn, :sn, [:call, [:vcall, :m], :sn, [:array, [:lit, /s/]
    ]]],
    {:warnings=>["(string):1: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "sn = m.sn :sen" =>
    [[:lasgn, :sn, [:call, [:vcall, :m], :sn, [:array, [:lit, :sen]
    ]]]],

  "sn = m.sn ?s" =>
    [[:lasgn, :sn, [:call, [:vcall, :m], :sn, [:array, [:lit, 115]
    ]]]],

  "sn = m::sn :sen" =>
    [[:lasgn, :sn, [:call, [:vcall, :m], :sn, [:array, [:lit, :sen]
    ]]]],

  "sources.each { |src| ant.src :path => src }" =>
    [[:iter, [:call, [:vcall, :sources], :each], [:dasgn_curr,
     :src], [:call, [:vcall, :ant], :src, [:array, [:hash, [:lit,
     :path], [:dvar, :src]]]]]],

  "st = F f rescue p" =>
    [[:rescue, [:lasgn, :st, [:fcall, :F, [:array, [:vcall, :f]]
    ]], [:resbody, nil, [:vcall, :p]]]],

  "st = a.F f rescue p" =>
    [[:rescue, [:lasgn, :st, [:call, [:vcall, :a], :F, [:array, [
    :vcall, :f]]]], [:resbody, nil, [:vcall, :p]]]],

  "st = a::F f rescue p" =>
    [[:rescue, [:lasgn, :st, [:call, [:vcall, :a], :F, [:array, [
    :vcall, :f]]]], [:resbody, nil, [:vcall, :p]]]],

  "stat = File.stat f rescue error" =>
    [[:rescue, [:lasgn, :stat, [:call, [:const, :File], :stat, [
    :array, [:vcall, :f]]]], [:resbody, nil, [:vcall, :error]]]],

  "status, err_args = Documeh_status{fcgi_state = 3; docespond\
 do doc_response =fcgi_state =  1; end }" =>
    [[:masgn, [:array, [:lasgn, :status], [:lasgn, :err_args]],
     nil, [:to_ary, [:iter, [:fcall, :Documeh_status], nil, [
    :block, [:dasgn_curr, :fcgi_state], [:dasgn_curr, :fcgi_state, [
    :lit, 3]], [:iter, [:fcall, :docespond], nil, [:dasgn_curr,
     :doc_response, [:dasgn, :fcgi_state, [:lit, 1]]]]]]]]],

  "super" => [[:zsuper]],
  "super *c" =>
    [[:super, [:splat, [:vcall, :c]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "super a,b,*c do d end" =>
    [[:iter, [:super, [:argscat, [:array, [:vcall, :a], [:vcall,
     :b]], [:vcall, :c]]], nil, [:vcall, :d]]],

  "super b,*c" =>
    [[:super, [:argscat, [:array, [:vcall, :b]], [:vcall, :c]]]],

  "super(&a)" => [[:block_pass, [:vcall, :a], [:super]]],
  "super()" => [[:super]],
  "super(*a)" => [[:super, [:splat, [:vcall, :a]]]],
  "super(*a,&b)" =>
    [[:block_pass, [:vcall, :b], [:super, [:splat, [:vcall, :a]]
    ]]],

  "super(1)" => [[:super, [:array, [:lit, 1]]]],
  "super(1,&b)" =>
    [[:block_pass, [:vcall, :b], [:super, [:array, [:lit, 1]]]]],

  "super(1,*b)" =>
    [[:super, [:argscat, [:array, [:lit, 1]], [:vcall, :b]]]],

  "super(1,*b,&c)" =>
    [[:block_pass, [:vcall, :c], [:super, [:argscat, [:array, [
    :lit, 1]], [:vcall, :b]]]]],

  "super(1,2)" => [[:super, [:array, [:lit, 1], [:lit, 2]]]],
  "super(1,2,&c)" =>
    [[:block_pass, [:vcall, :c], [:super, [:array, [:lit, 1], [
    :lit, 2]]]]],

  "super(1,2,*c)" =>
    [[:super, [:argscat, [:array, [:lit, 1], [:lit, 2]], [:vcall,
     :c]]]],

  "super(1,2,*c,&d)" =>
    [[:block_pass, [:vcall, :d], [:super, [:argscat, [:array, [
    :lit, 1], [:lit, 2]], [:vcall, :c]]]]],

  "super(1,2,3)" =>
    [[:super, [:array, [:lit, 1], [:lit, 2], [:lit, 3]]]],

  "super[]" => [[:call, [:zsuper], :[]]],
  "t ?6e0 : 5" => [[:if, [:vcall, :t], [:lit, 6.0], [:lit, 5]]],
  "t p = m do 666 end" =>
    [[:iter, [:fcall, :t, [:array, [:lasgn, :p, [:vcall, :m]]]],
     nil, [:lit, 666]]],

  "throw * @com_disk" =>
    [[:call, [:vcall, :throw], :*, [:array, [:ivar, :@com_disk]]
    ]],

  "throw - @com_disk" =>
    [[:call, [:vcall, :throw], :-, [:array, [:ivar, :@com_disk]]
    ]],

  "true" => [[:true]],
  "true;a" =>
    [[:block, [:true], [:vcall, :a]],
    {:warnings=>["(string):1: warning: useless use of true in void context"]}],

  "tuf while buf=sread 4096" => SyntaxError.new("(string):1: syntax error, unexpected tINTEGER, expecting kDO or '{' or '('\ntuf while buf=sread 4096\n                        ^"),
  "undef * com_disk" => SyntaxError.new("(string):1: syntax error, unexpected tIDENTIFIER, expecting $end\nundef * com_disk\n                ^"),
  "undef - com_disk" => SyntaxError.new("(string):1: syntax error, unexpected tIDENTIFIER, expecting $end\nundef - com_disk\n                ^"),
  "\
undef :\"
__END__
\#{bar}\"" =>
    [[:undef, [:dsym, "\n__END__\n", [:evstr, [:vcall, :bar]]]]],

  "undef :\"foo\#{bar}\", :\"baz\#{quux}\"" =>
    [[:block, [:undef, [:dsym, "foo", [:evstr, [:vcall, :bar]]]
    ], [:undef, [:dsym, "baz", [:evstr, [:vcall, :quux]]]]]],

  "undef :*,<<,/,[]" =>
    [[:block, [:undef, [:lit, :*]], [:undef, [:lit, :<<]], [
    :undef, [:lit, :/]], [:undef, [:lit, :[]]]]],

  "undef :a" => [[:undef, [:lit, :a]]],
  "undef :a,:b" =>
    [[:block, [:undef, [:lit, :a]], [:undef, [:lit, :b]]]],

  "undef :a,:b,:c" =>
    [[:block, [:undef, [:lit, :a]], [:undef, [:lit, :b]], [:undef, [
    :lit, :c]]]],

  "undef :~@" => [[:undef, [:lit, :~]]],
  "\
undef 
A" =>
    [[:undef, [:lit, :A]]],

  "\
undef 
A,B" =>
    [[:block, [:undef, [:lit, :A]], [:undef, [:lit, :B]]]],

  "undef a" => [[:undef, [:lit, :a]]],
  "undef a,b" =>
    [[:block, [:undef, [:lit, :a]], [:undef, [:lit, :b]]]],

  "undef a,b,c" =>
    [[:block, [:undef, [:lit, :a]], [:undef, [:lit, :b]], [:undef, [
    :lit, :c]]]],

  "undef ~@" => [[:undef, [:lit, :~]]],
  "unless * @com_disk;end" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR\nunless * @com_disk;end\n        ^"),
  "unless - @com_disk;end" =>
    [[:if, [:call, [:ivar, :@com_disk], :-@], nil, nil]],

  "unless a; b else c end" =>
    [[:if, [:vcall, :a], [:vcall, :c], [:vcall, :b]]],

  "unless a; b end" => [[:if, [:vcall, :a], nil, [:vcall, :b]]],
  "unless not a; b end" =>
    [[:if, [:vcall, :a], [:vcall, :b], nil]],

  "unless p (1).m; end" =>
    [[:if, [:call, [:fcall, :p, [:array, [:lit, 1]]], :m], nil,
     nil],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "unless v; else return ([vUew]).e  end" =>
    [[:if, [:vcall, :v], [:return, [:call, [:array, [:vcall, :vUew]
    ], :e]], nil]],

  "until * @com_disk;end" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR\nuntil * @com_disk;end\n       ^"),
  "until - @com_disk;end" =>
    [[:until, [:call, [:ivar, :@com_disk], :-@], nil, true]],

  "until a; b end" => [[:until, [:vcall, :a], [:vcall, :b], true]],
  "until false do; end" => [[:until, [:false], nil, true]],
  "until false; break end" => [[:until, [:false], [:break], true]],
  "until not a; b end" =>
    [[:while, [:vcall, :a], [:vcall, :b], true]],

  "until p (1).m; end" =>
    [[:until, [:call, [:fcall, :p, [:array, [:lit, 1]]], :m], nil,
     true],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "while (((((((((((((((((((((((((((((((((((((((((((((((((((((\
((((((((((false)))))))))))))))))))))))))))))))))))))))))))))\
)))))))))))))))))) do; end" =>
    [[:while, [:false], nil, true]],

  "while (((((((((((((((((((((((((((((((false)))))))))))))))))\
)))))))))))))) do; end" =>
    [[:while, [:false], nil, true]],

  "while * @com_disk;end" => SyntaxError.new("(string):1: syntax error, unexpected tSTAR\nwhile * @com_disk;end\n       ^"),
  "while - @com_disk;end" =>
    [[:while, [:call, [:ivar, :@com_disk], :-@], nil, true]],

  "while a do end" => [[:while, [:vcall, :a], nil, true]],
  "while a: end" => [[:while, [:vcall, :a], nil, true]],
  "while a; b end" => [[:while, [:vcall, :a], [:vcall, :b], true]],
  "while a; end" => [[:while, [:vcall, :a], nil, true]],
  "while begin; a; rescue b; end ;  end" =>
    [[:while, [:rescue, [:vcall, :a], [:resbody, [:array, [
    :vcall, :b]]]], nil, true]],

  "while false do end" => [[:while, [:false], nil, true]],
  "while false do; end" => [[:while, [:false], nil, true]],
  "while false; break *[3,4] end" =>
    [[:while, [:false], [:break, [:svalue, [:splat, [:array, [
    :lit, 3], [:lit, 4]]]]], true]],

  "while false; break 1,2 end" =>
    [[:while, [:false], [:break, [:array, [:lit, 1], [:lit, 2]]
    ], true]],

  "while false; break 1,2,*[3,4] end" =>
    [[:while, [:false], [:break, [:argscat, [:array, [:lit, 1], [
    :lit, 2]], [:array, [:lit, 3], [:lit, 4]]]], true]],

  "while false; break end" => [[:while, [:false], [:break], true]],
  "while false; break(1) end" =>
    [[:while, [:false], [:break, [:lit, 1]], true]],

  "while false; next end" => [[:while, [:false], [:next], true]],
  "while false; redo end" => [[:while, [:false], [:redo], true]],
  "while false; retry end" => [[:while, [:false], [:retry], true]],
  "while not a; b end" =>
    [[:until, [:vcall, :a], [:vcall, :b], true]],

  "while p (1).m; end" =>
    [[:while, [:call, [:fcall, :p, [:array, [:lit, 1]]], :m], nil,
     true],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "wwww,eeee=1,2" =>
    [[:masgn, [:array, [:lasgn, :wwww], [:lasgn, :eeee]], nil, [
    :array, [:lit, 1], [:lit, 2]]]],

  "\
x do
  a,b,* = [1,2,3,4,5,6,7,8]
  p a,b
  a,b, = [1,2,3,4,5,6,7,8]
  p a,b
  a,b = [1,2,3,4,5,6,7,8]
  p a,b
  a,*b = [1,2,3,4,5,6,7,8]
  p a,b
  a,b,*c=[1,2,3,4,5,6,7,8]
  a,b,* c=[1,2,3,4,5,6,7,8]
end\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:masgn, [:array, [
    :dasgn_curr, :a], [:dasgn_curr, :b]], [:splat], [:to_ary, [
    :array, [:lit, 1], [:lit, 2], [:lit, 3], [:lit, 4], [:lit, 5], [
    :lit, 6], [:lit, 7], [:lit, 8]]]], [:fcall, :p, [:array, [
    :dvar, :a], [:dvar, :b]]], [:masgn, [:array, [:dasgn_curr, :a], [
    :dasgn_curr, :b]], nil, [:to_ary, [:array, [:lit, 1], [:lit,
     2], [:lit, 3], [:lit, 4], [:lit, 5], [:lit, 6], [:lit, 7], [
    :lit, 8]]]], [:fcall, :p, [:array, [:dvar, :a], [:dvar, :b]]
    ], [:masgn, [:array, [:dasgn_curr, :a], [:dasgn_curr, :b]],
     nil, [:to_ary, [:array, [:lit, 1], [:lit, 2], [:lit, 3], [
    :lit, 4], [:lit, 5], [:lit, 6], [:lit, 7], [:lit, 8]]]], [
    :fcall, :p, [:array, [:dvar, :a], [:dvar, :b]]], [:masgn, [
    :array, [:dasgn_curr, :a]], [:dasgn_curr, :b], [:to_ary, [
    :array, [:lit, 1], [:lit, 2], [:lit, 3], [:lit, 4], [:lit, 5], [
    :lit, 6], [:lit, 7], [:lit, 8]]]], [:fcall, :p, [:array, [
    :dvar, :a], [:dvar, :b]]], [:masgn, [:array, [:dasgn_curr, :a], [
    :dasgn_curr, :b]], [:dasgn_curr, :c], [:to_ary, [:array, [
    :lit, 1], [:lit, 2], [:lit, 3], [:lit, 4], [:lit, 5], [:lit,
     6], [:lit, 7], [:lit, 8]]]], [:masgn, [:array, [:dasgn_curr,
     :a], [:dasgn_curr, :b]], [:dasgn_curr, :c], [:to_ary, [
    :array, [:lit, 1], [:lit, 2], [:lit, 3], [:lit, 4], [:lit, 5], [
    :lit, 6], [:lit, 7], [:lit, 8]]]]]]],

  "\
x do
  a=1
  p(a +77)
  def hhh(a=(1+2)) a end
end\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :a, [:lit,
     1]], [:fcall, :p, [:array, [:call, [:dvar, :a], :+, [:array, [
    :lit, 77]]]]], [:defn, :hhh, [:scope, [:block, [:args, :a, [
    :block, [:lasgn, :a, [:call, [:lit, 1], :+, [:array, [:lit,
     2]]]]]], [:lvar, :a]]]]]]],

  "\
x do
 method_src = c.compile(template, (HtmlCompiler::AnyData.new)).join(\"\\n\") +
    \"\\n# generated by PartsTemplate::compile_partstemplate at \#{Time.new}\\n\"
 rescu -1
end\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :method_src, [
    :call, [:call, [:call, [:vcall, :c], :compile, [:array, [
    :vcall, :template], [:call, [:colon2, [:const, :HtmlCompiler],
     :AnyData], :new]]], :join, [:array, [:str, "\n"]]], :+, [
    :array, [:dstr, "\n# generated by PartsTemplate::compile_partstemplate at ", [
    :evstr, [:call, [:const, :Time], :new]], [:str, "\n"]]]]], [
    :fcall, :rescu, [:array, [:lit, -1]]]]],
    {:warnings=>["(string):4: warning: ambiguous first argument; put parentheses or even spaces"]}],

  "\
x do
a='a'
class <<a
  def foobar
     self*101
  end
  alias    eql?    ==
end
p a.foobar
end\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :a, [:str,
     "a"]], [:sclass, [:dvar, :a], [:scope, [:block, [:defn, :foobar, [
    :scope, [:block, [:args], [:call, [:self], :*, [:array, [
    :lit, 101]]]]]], [:alias, [:lit, :eql?], [:lit, :==]]]]], [
    :fcall, :p, [:array, [:call, [:dvar, :a], :foobar]]]]]],

  "\
x do
x, (*y) = [:x, :y, :z]
p x
p y
x, *y = [:x, :y, :z]
p x
p y
x, * = [:x, :y, :z]
p x
end\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:masgn, [:array, [
    :dasgn_curr, :x], [:masgn, nil, [:dasgn_curr, :y], nil]], nil, [
    :to_ary, [:array, [:lit, :x], [:lit, :y], [:lit, :z]]]], [
    :fcall, :p, [:array, [:dvar, :x]]], [:fcall, :p, [:array, [
    :dvar, :y]]], [:masgn, [:array, [:dasgn_curr, :x]], [
    :dasgn_curr, :y], [:to_ary, [:array, [:lit, :x], [:lit, :y],
     [:lit, :z]]]], [:fcall, :p, [:array, [:dvar, :x]]], [:fcall,
     :p, [:array, [:dvar, :y]]], [:masgn, [:array, [:dasgn_curr,
     :x]], [:splat], [:to_ary, [:array, [:lit, :x], [:lit, :y], [
    :lit, :z]]]], [:fcall, :p, [:array, [:dvar, :x]]]]]],

  "x if /f/../o/" =>
    [[:if, [:flip2, [:match, [:lit, /f/]], [:match, [:lit, /o/]]
    ], [:vcall, :x], nil],
    {:warnings=>["(string):1: warning: regex literal in condition", "(string):1: warning: regex literal in condition"]}],

  "\
x {
  class<<self
   alias q p
   alias r p
   alias s p
  end
  p(q,r,s)
  q %(1)
  r %(1)
  s %(1)
  p (q,r,s)
  q %(1)
  r %(1)
  s %(1)
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:sclass, [:self], [
    :scope, [:block, [:alias, [:lit, :q], [:lit, :p]], [:alias, [
    :lit, :r], [:lit, :p]], [:alias, [:lit, :s], [:lit, :p]]]]],
     [:fcall, :p, [:array, [:vcall, :q], [:vcall, :r], [:vcall,
     :s]]], [:fcall, :q, [:array, [:str, "1"]]], [:fcall, :r, [
    :array, [:str, "1"]]], [:fcall, :s, [:array, [:str, "1"]]], [
    :fcall, :p, [:array, [:vcall, :q], [:vcall, :r], [:vcall, :s]
    ]], [:fcall, :q, [:array, [:str, "1"]]], [:fcall, :r, [:array, [
    :str, "1"]]], [:fcall, :s, [:array, [:str, "1"]]]]],
    {:warnings=>["(string):11: warning: don't put space before argument parentheses"]}],

  "\
x {
  h={:a=>(foo=100)}
  p( foo %(5))
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :h, [
    :hash, [:lit, :a], [:dasgn_curr, :foo, [:lit, 100]]]], [
    :fcall, :p, [:array, [:call, [:dvar, :foo], :%, [:array, [
    :lit, 5]]]]]]]],

  "x.I = 1" =>
    [[:attrasgn, [:vcall, :x], :I=, [:array, [:lit, 1]]]],

  "x::I = 1" => [[:cdecl, [:colon2, [:vcall, :x], :I], [:lit, 1]]],
  "xxx=0; xxx ?'':\"\" " =>
    [[:block, [:lasgn, :xxx, [:lit, 0]], [:if, [:lvar, :xxx], [
    :str, ""], [:str, ""]]]],

  "xxx=0; xxx ?111:222" =>
    [[:block, [:lasgn, :xxx, [:lit, 0]], [:if, [:lvar, :xxx], [
    :lit, 111], [:lit, 222]]]],

  "xxx=yyy=zzz=0; xxx ?yyy:zzz" =>
    [[:block, [:lasgn, :xxx, [:lasgn, :yyy, [:lasgn, :zzz, [:lit,
     0]]]], [:if, [:lvar, :xxx], [:lvar, :yyy], [:lvar, :zzz]]]],

  "\
x{
      without_creating=widgetname=nil
      if without_creating && !widgetname #foo
        fail ArgumentError,
             \"if set 'without_creating' to true, need to define 'widgetname'\"
      end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :without_creating, [
    :dasgn_curr, :widgetname, [:nil]]], [:if, [:and, [
    :dvar, :without_creating], [:not, [:dvar, :widgetname]]], [
    :fcall, :fail, [:array, [:const, :ArgumentError], [
    :str, "if set 'without_creating' to true, need to define 'widgetname'"]
    ]], nil]]]],

  "\
x{
  a,b,c,(d,e)=1,2,3,[4,5]
  p a %(4)
  p c %(4)
  p d %(4)
  p e %(4)
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:masgn, [:array, [
    :dasgn_curr, :a], [:dasgn_curr, :b], [:dasgn_curr, :c], [
    :masgn, [:array, [:dasgn_curr, :d], [:dasgn_curr, :e]], nil,
     nil]], nil, [:array, [:lit, 1], [:lit, 2], [:lit, 3], [
    :array, [:lit, 4], [:lit, 5]]]], [:fcall, :p, [:array, [
    :call, [:dvar, :a], :%, [:array, [:lit, 4]]]]], [:fcall, :p,
     [:array, [:call, [:dvar, :c], :%, [:array, [:lit, 4]]]]], [
    :fcall, :p, [:array, [:call, [:dvar, :d], :%, [:array, [:lit,
     4]]]]], [:fcall, :p, [:array, [:call, [:dvar, :e], :%, [
    :array, [:lit, 4]]]]]]]],

  "\
x{
  a0=9
  p Foou.new.[]a0      #value
  p Foou.new.[] a0     #value
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :a0, [
    :lit, 9]], [:fcall, :p, [:array, [:call, [:call, [:const,
     :Foou], :new], :[], [:array, [:dvar, :a0]]]]], [:fcall, :p,
     [:array, [:call, [:call, [:const, :Foou], :new], :[], [
    :array, [:dvar, :a0]]]]]]],
    {:warnings=>["(string):3: warning: parenthesize argument(s) for future version", "(string):4: warning: parenthesize argument(s) for future version"]}],

  "\
x{
  a=5
  p p +5
  p a +5
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :a, [:lit,
     5]], [:fcall, :p, [:array, [:fcall, :p, [:array, [:lit, 5]]
    ]]], [:fcall, :p, [:array, [:call, [:dvar, :a], :+, [:array,
     [:lit, 5]]]]]]],
    {:warnings=>["(string):3: warning: ambiguous first argument; put parentheses or even spaces", "(string):3: warning: parenthesize argument(s) for future version"]}],

  "\
x{
  a=b=c=0
  a ? b:c
  a ?b:c
  p(a ? b:c)
  p(a ?b:c)
  p(a ?:r:c)
  p(a ? :r:c)
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :a, [
    :dasgn_curr, :b, [:dasgn_curr, :c, [:lit, 0]]]], [:if, [
    :dvar, :a], [:dvar, :b], [:dvar, :c]], [:if, [:dvar, :a], [
    :dvar, :b], [:dvar, :c]], [:fcall, :p, [:array, [:if, [:dvar,
     :a], [:dvar, :b], [:dvar, :c]]]], [:fcall, :p, [:array, [
    :if, [:dvar, :a], [:dvar, :b], [:dvar, :c]]]], [:fcall, :p, [
    :array, [:if, [:dvar, :a], [:lit, :r], [:dvar, :c]]]], [
    :fcall, :p, [:array, [:if, [:dvar, :a], [:lit, :r], [:dvar,
     :c]]]]]]],

  "\
x{
  def %(n) to_s+\"%\#{n}\" end
  def bill(x) x end
  p(bill %(22))
  begin sdjkfsjkdfsd; rescue Object => bill; p(bill %(22)) end  
  p(bill %(22))
  undef %
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:defn, :%, [:scope, [
    :block, [:args, :n], [:call, [:vcall, :to_s], :+, [:array, [
    :dstr, "%", [:evstr, [:lvar, :n]]]]]]]], [:defn, :bill, [
    :scope, [:block, [:args, :x], [:lvar, :x]]]], [:fcall, :p, [
    :array, [:fcall, :bill, [:array, [:str, "22"]]]]], [:rescue,
     [:vcall, :sdjkfsjkdfsd], [:resbody, [:array, [:const, :Object]
    ], [:block, [:dasgn_curr, :bill, [:gvar, :$!]], [:fcall, :p,
     [:array, [:call, [:dvar, :bill], :%, [:array, [:lit, 22]]]]
    ]]]], [:fcall, :p, [:array, [:call, [:dvar, :bill], :%, [
    :array, [:lit, 22]]]]], [:undef, [:lit, :%]]]],
    {:warnings=>["(string):4: warning: parenthesize argument(s) for future version"]}],

  "\
x{
  def bob(x) x end
  p(bob %(22))
  for bob in [100] do p(bob %(22)) end
  p(bob %(22))
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:defn, :bob, [:scope, [
    :block, [:args, :x], [:lvar, :x]]]], [:fcall, :p, [:array, [
    :fcall, :bob, [:array, [:str, "22"]]]]], [:for, [:array, [
    :lit, 100]], [:dasgn_curr, :bob], [:fcall, :p, [:array, [
    :call, [:dvar, :bob], :%, [:array, [:lit, 22]]]]]], [:fcall,
     :p, [:array, [:call, [:dvar, :bob], :%, [:array, [:lit, 22]
    ]]]]]],
    {:warnings=>["(string):3: warning: parenthesize argument(s) for future version"]}],

  "\
x{
  def yy;yield end
  block=proc{p \"blah  blah\"}
  yy &block
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:defn, :yy, [:scope, [
    :block, [:args], [:yield]]]], [:dasgn_curr, :block, [:iter, [
    :fcall, :proc], nil, [:fcall, :p, [:array, [:str, "blah  blah"]
    ]]]], [:block_pass, [:dvar, :block], [:fcall, :yy]]]],
    {:warnings=>["(string):4: warning: `&' interpreted as argument prefix"]}],

  "\
x{
  f0000=1
  def f0000; end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :f0000, [
    :lit, 1]], [:defn, :f0000, [:scope, [:block, [:args], [:nil]
    ]]]]]],

  "\
x{
  foo bar=>baz
  bar %(1)
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:fcall, :foo, [:array,
     [:hash, [:vcall, :bar], [:vcall, :baz]]]], [:fcall, :bar, [
    :array, [:str, "1"]]]]]],

  "\
x{
  h={}
  h.default=:foo
  p def h.default= v; p @v=v;v end
  p def (h).default= v; p @v=v;v end
  p def (h=\"foobar\").default= v; p @v=v;v end
  p h
  p h.default=:b
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :h, [
    :hash]], [:attrasgn, [:dvar, :h], :default=, [:array, [:lit,
     :foo]]], [:fcall, :p, [:array, [:defs, [:dvar, :h], :default=, [
    :scope, [:block, [:args, :v], [:fcall, :p, [:array, [:iasgn,
     :@v, [:lvar, :v]]]], [:lvar, :v]]]]]], [:fcall, :p, [:array, [
    :defs, [:dvar, :h], :default=, [:scope, [:block, [:args, :v], [
    :fcall, :p, [:array, [:iasgn, :@v, [:lvar, :v]]]], [:lvar, :v]
    ]]]]], [:fcall, :p, [:array, [:defs, [:dasgn_curr, :h, [:str,
     "foobar"]], :default=, [:scope, [:block, [:args, :v], [
    :fcall, :p, [:array, [:iasgn, :@v, [:lvar, :v]]]], [:lvar, :v]
    ]]]]], [:fcall, :p, [:array, [:dvar, :h]]], [:fcall, :p, [
    :array, [:attrasgn, [:dvar, :h], :default=, [:array, [:lit,
     :b]]]]]]],
    {:warnings=>["(string):4: warning: void value expression", "(string):5: warning: void value expression", "(string):6: warning: void value expression"]}],

  "\
x{
#class, module, and def should temporarily hide local variables
def mopsdfjskdf arg; arg*2 end
mopsdfjskdf=5
 class C
 p mopsdfjskdf %(3)    #calls method
 end
module M
 p mopsdfjskdf %(4)    #calls method
end
 def d
 p mopsdfjskdf %(5)    #calls method
 end
p d
p mopsdfjskdf %(6)     #reads variable
p proc{mopsdfjskdf %(7)}[] #reads variable
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:defn, :mopsdfjskdf, [
    :scope, [:block, [:args, :arg], [:call, [:lvar, :arg], :*, [
    :array, [:lit, 2]]]]]], [:dasgn_curr, :mopsdfjskdf, [:lit, 5]
    ], [:class, :C, nil, [:scope, [:fcall, :p, [:array, [:fcall,
     :mopsdfjskdf, [:array, [:str, "3"]]]]]]], [:module, :M, [
    :scope, [:fcall, :p, [:array, [:fcall, :mopsdfjskdf, [:array, [
    :str, "4"]]]]]]], [:defn, :d, [:scope, [:block, [:args], [
    :fcall, :p, [:array, [:fcall, :mopsdfjskdf, [:array, [:str,
     "5"]]]]]]]], [:fcall, :p, [:array, [:vcall, :d]]], [:fcall,
     :p, [:array, [:call, [:dvar, :mopsdfjskdf], :%, [:array, [
    :lit, 6]]]]], [:fcall, :p, [:array, [:call, [:iter, [:fcall,
     :proc], nil, [:call, [:dvar, :mopsdfjskdf], :%, [:array, [
    :lit, 7]]]], :[]]]]]],
    {:warnings=>["(string):6: warning: parenthesize argument(s) for future version", "(string):9: warning: parenthesize argument(s) for future version", "(string):12: warning: parenthesize argument(s) for future version"]}],

  "\
x{
c=j=0
until while j<10 do j+=1 end.nil? do p 'pppppppppp' end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :c, [
    :dasgn_curr, :j, [:lit, 0]]], [:until, [:call, [:while, [
    :call, [:dvar, :j], :<, [:array, [:lit, 10]]], [:dasgn_curr,
     :j, [:call, [:dvar, :j], :+, [:array, [:lit, 1]]]], true],
     :nil?], [:fcall, :p, [:array, [:str, "pppppppppp"]]], true]
    ]]],

  "\
x{
for i in (
[44,55,66,77,88]) do p i**Math.sqrt(i) end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:for, [:array, [:lit, 44], [
    :lit, 55], [:lit, 66], [:lit, 77], [:lit, 88]], [:dasgn_curr,
     :i], [:fcall, :p, [:array, [:call, [:dvar, :i], :**, [:array, [
    :call, [:const, :Math], :sqrt, [:array, [:dvar, :i]]]]]]]]]],

  "\
x{
for i in (begin
[44,55,66,77,88] end) do p i**Math.sqrt(i) end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:for, [:array, [:lit, 44], [
    :lit, 55], [:lit, 66], [:lit, 77], [:lit, 88]], [:dasgn_curr,
     :i], [:fcall, :p, [:array, [:call, [:dvar, :i], :**, [:array, [
    :call, [:const, :Math], :sqrt, [:array, [:dvar, :i]]]]]]]]]],

  "\
x{
for i in (c;
[44,55,66,77,88]) do p i**Math.sqrt(i) end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:for, [:block, [:vcall, :c], [
    :array, [:lit, 44], [:lit, 55], [:lit, 66], [:lit, 77], [
    :lit, 88]]], [:dasgn_curr, :i], [:fcall, :p, [:array, [:call, [
    :dvar, :i], :**, [:array, [:call, [:const, :Math], :sqrt, [
    :array, [:dvar, :i]]]]]]]]]],

  "\
x{
for i in \\
[44,55,66,77,88] do p i**Math.sqrt(i) end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:for, [:array, [:lit, 44], [
    :lit, 55], [:lit, 66], [:lit, 77], [:lit, 88]], [:dasgn_curr,
     :i], [:fcall, :p, [:array, [:call, [:dvar, :i], :**, [:array, [
    :call, [:const, :Math], :sqrt, [:array, [:dvar, :i]]]]]]]]]],

  "\
x{
for i in if false then foob12345; else
[44,55,66,77,88] end do p i**Math.sqrt(i) end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:for, [:if, [:false], [:vcall,
     :foob12345], [:array, [:lit, 44], [:lit, 55], [:lit, 66], [
    :lit, 77], [:lit, 88]]], [:dasgn_curr, :i], [:fcall, :p, [
    :array, [:call, [:dvar, :i], :**, [:array, [:call, [:const,
     :Math], :sqrt, [:array, [:dvar, :i]]]]]]]]]],

  "\
x{
for i in if false then foob12345;
else [44,55,66,77,88] end do p i**Math.sqrt(i) end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:for, [:if, [:false], [:vcall,
     :foob12345], [:array, [:lit, 44], [:lit, 55], [:lit, 66], [
    :lit, 77], [:lit, 88]]], [:dasgn_curr, :i], [:fcall, :p, [
    :array, [:call, [:dvar, :i], :**, [:array, [:call, [:const,
     :Math], :sqrt, [:array, [:dvar, :i]]]]]]]]]],

  "\
x{
for i in if false then
foob12345; else [44,55,66,77,88] end do p i**Math.sqrt(i) end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:for, [:if, [:false], [:vcall,
     :foob12345], [:array, [:lit, 44], [:lit, 55], [:lit, 66], [
    :lit, 77], [:lit, 88]]], [:dasgn_curr, :i], [:fcall, :p, [
    :array, [:call, [:dvar, :i], :**, [:array, [:call, [:const,
     :Math], :sqrt, [:array, [:dvar, :i]]]]]]]]]],

  "\
x{
for i in if false
foob12345; else [44,55,66,77,88] end do p i**Math.sqrt(i) end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:for, [:if, [:false], [:vcall,
     :foob12345], [:array, [:lit, 44], [:lit, 55], [:lit, 66], [
    :lit, 77], [:lit, 88]]], [:dasgn_curr, :i], [:fcall, :p, [
    :array, [:call, [:dvar, :i], :**, [:array, [:call, [:const,
     :Math], :sqrt, [:array, [:dvar, :i]]]]]]]]]],

  "\
x{
for i in if
false then foob12345; else [44,55,66,77,88] end do p i**Math.sqrt(i) end
}\

" =>
    [[:iter, [:fcall, :x], nil, [:for, [:if, [:false], [:vcall,
     :foob12345], [:array, [:lit, 44], [:lit, 55], [:lit, 66], [
    :lit, 77], [:lit, 88]]], [:dasgn_curr, :i], [:fcall, :p, [
    :array, [:call, [:dvar, :i], :**, [:array, [:call, [:const,
     :Math], :sqrt, [:array, [:dvar, :i]]]]]]]]]],

  "x{a=b do end}     " =>
    [[:iter, [:fcall, :x], nil, [:dasgn_curr, :a, [:iter, [
    :fcall, :b], nil]]]],

  "x{b=1;a b do end}    " =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :b, [:lit,
     1]], [:iter, [:fcall, :a, [:array, [:dvar, :b]]], nil]]]],

  "x{b=1;a=b do end} " =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :b, [:lit,
     1]], [:dasgn_curr, :a, [:iter, [:fcall, :b], nil]]]]],

  "x{q=1;def q.foo; end}  " =>
    [[:iter, [:fcall, :x], nil, [:block, [:dasgn_curr, :q, [:lit,
     1]], [:defs, [:dvar, :q], :foo, [:scope, [:args]]]]]],

  "y,*a0= begin a end rescue b0" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :y]], [:lasgn, :a0], [
    :to_ary, [:vcall, :a]]], [:resbody, nil, [:vcall, :b0]]]],

  "y,a0,* = begin a end rescue b0" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :y], [:lasgn, :a0]], [
    :splat], [:to_ary, [:vcall, :a]]], [:resbody, nil, [:vcall,
     :b0]]]],

  "y,a0,*j= begin a end rescue b0" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :y], [:lasgn, :a0]], [
    :lasgn, :j], [:to_ary, [:vcall, :a]]], [:resbody, nil, [
    :vcall, :b0]]]],

  "y,a0,= begin a end rescue b0" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :y], [:lasgn, :a0]], nil, [
    :to_ary, [:vcall, :a]]], [:resbody, nil, [:vcall, :b0]]]],

  "y,a0= begin a end rescue b0" =>
    [[:rescue, [:masgn, [:array, [:lasgn, :y], [:lasgn, :a0]], nil, [
    :to_ary, [:vcall, :a]]], [:resbody, nil, [:vcall, :b0]]]],

  "yield" => [[:yield]],
  "yield * @com_disk" =>
    [[:call, [:yield], :*, [:array, [:ivar, :@com_disk]]]],

  "yield *a=r" =>
    [[:yield, [:splat, [:lasgn, :a, [:vcall, :r]]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "yield *c" =>
    [[:yield, [:splat, [:vcall, :c]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix"]}],

  "yield *p (1).m" =>
    [[:yield, [:splat, [:call, [:fcall, :p, [:array, [:lit, 1]]
    ], :m]]],
    {:warnings=>["(string):1: warning: `*' interpreted as argument prefix", "(string):1: warning: don't put space before argument parentheses"]}],

  "yield - @com_disk" =>
    [[:call, [:yield], :-, [:array, [:ivar, :@com_disk]]]],

  "yield [1]" => [[:yield, [:array, [:lit, 1]], true]],
  "yield []" => [[:yield, [:zarray], true]],
  "yield [a_i, *p] " =>
    [[:yield, [:argscat, [:array, [:vcall, :a_i]], [:vcall, :p]]
    ]],

  "yield a rescue b" =>
    [[:rescue, [:yield, [:vcall, :a]], [:resbody, nil, [:vcall,
     :b]]]],

  "yield a,b,*c do d end" =>
    [[:iter, [:yield, [:argscat, [:array, [:vcall, :a], [:vcall,
     :b]], [:vcall, :c]]], nil, [:vcall, :d]]],

  "yield b,*c" =>
    [[:yield, [:argscat, [:array, [:vcall, :b]], [:vcall, :c]]]],

  "yield()" => [[:yield]],
  "yield(*a)" => [[:yield, [:splat, [:vcall, :a]]]],
  "yield(1)" => [[:yield, [:lit, 1]]],
  "yield(1,*b)" =>
    [[:yield, [:argscat, [:array, [:lit, 1]], [:vcall, :b]]]],

  "yield(1,2)" => [[:yield, [:array, [:lit, 1], [:lit, 2]]]],
  "yield(1,2,*c)" =>
    [[:yield, [:argscat, [:array, [:lit, 1], [:lit, 2]], [:vcall,
     :c]]]],

  "yield(1,2,3)" =>
    [[:yield, [:array, [:lit, 1], [:lit, 2], [:lit, 3]]]],

  "y{|z[begin; a; rescue b; end]|c}" =>
    [[:iter, [:fcall, :y], [:attrasgn, [:vcall, :z], :[]=, [
    :array, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]]]], [:vcall, :c]]],

  "z = valueo_s rescue \"?\"" =>
    [[:lasgn, :z, [:rescue, [:vcall, :valueo_s], [:resbody, nil,
     [:str, "?"]]]]],

  "z if a || /c..d/" =>
    [[:if, [:or, [:vcall, :a], [:match, [:lit, /c..d/]]], [
    :vcall, :z], nil],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "z if a || b or /c..d/" =>
    [[:if, [:or, [:vcall, :a], [:or, [:vcall, :b], [:match, [
    :lit, /c..d/]]]], [:vcall, :z], nil],
    {:warnings=>["(string):1: warning: regex literal in condition"]}],

  "z if a || b or c..d" =>
    [[:if, [:or, [:vcall, :a], [:or, [:vcall, :b], [:flip2, [
    :vcall, :c], [:vcall, :d]]]], [:vcall, :z], nil]],

  "z if a || c..d" =>
    [[:if, [:flip2, [:or, [:vcall, :a], [:vcall, :c]], [:vcall,
     :d]], [:vcall, :z], nil]],

  "z if c...p (1).m" =>
    [[:if, [:flip3, [:vcall, :c], [:call, [:fcall, :p, [:array, [
    :lit, 1]]], :m]], [:vcall, :z], nil],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "z if c..p (1).m" =>
    [[:if, [:flip2, [:vcall, :c], [:call, [:fcall, :p, [:array, [
    :lit, 1]]], :m]], [:vcall, :z], nil],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "z z z z z z do end" =>
    [[:iter, [:fcall, :z, [:array, [:fcall, :z, [:array, [:fcall,
     :z, [:array, [:fcall, :z, [:array, [:fcall, :z, [:array, [
    :vcall, :z]]]]]]]]]]], nil],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version", "(string):1: warning: parenthesize argument(s) for future version", "(string):1: warning: parenthesize argument(s) for future version", "(string):1: warning: parenthesize argument(s) for future version"]}],

  "z z z z z z {}" =>
    [[:fcall, :z, [:array, [:fcall, :z, [:array, [:fcall, :z, [
    :array, [:fcall, :z, [:array, [:fcall, :z, [:array, [:iter, [
    :fcall, :z], nil]]]]]]]]]]],
    {:warnings=>["(string):1: warning: parenthesize argument(s) for future version", "(string):1: warning: parenthesize argument(s) for future version", "(string):1: warning: parenthesize argument(s) for future version", "(string):1: warning: parenthesize argument(s) for future version"]}],

  "z(1){|a,a[b]| d}" =>
    [[:iter, [:fcall, :z, [:array, [:lit, 1]]], [:masgn, [:array, [
    :dasgn_curr, :a], [:attrasgn, [:dvar, :a], :[]=, [:array, [
    :vcall, :b]]]], nil, nil], [:vcall, :d]]],

  "z+a=b=c=d=f rescue g" =>
    [[:call, [:vcall, :z], :+, [:array, [:lasgn, :a, [:lasgn, :b, [
    :lasgn, :c, [:lasgn, :d, [:rescue, [:vcall, :f], [:resbody,
     nil, [:vcall, :g]]]]]]]]]],

  "z.a=~b" =>
    [[:call, [:call, [:vcall, :z], :a], :=~, [:array, [:vcall, :b]
    ]]],

  "z=a=b=1,d=2" =>
    [[:lasgn, :z, [:svalue, [:array, [:lasgn, :a, [:lasgn, :b, [
    :lit, 1]]], [:lasgn, :d, [:lit, 2]]]]]],

  "z[a=b=1,d=2]" =>
    [[:call, [:vcall, :z], :[], [:array, [:lasgn, :a, [:lasgn, :b, [
    :lit, 1]]], [:lasgn, :d, [:lit, 2]]]]],

  "z[a=b=1,d=2]=5" =>
    [[:attrasgn, [:vcall, :z], :[]=, [:array, [:lasgn, :a, [
    :lasgn, :b, [:lit, 1]]], [:lasgn, :d, [:lit, 2]], [:lit, 5]]
    ]],

  "z[begin; a; rescue b; end]" =>
    [[:call, [:vcall, :z], :[], [:array, [:rescue, [:vcall, :a],
     [:resbody, [:array, [:vcall, :b]]]]]]],

  "z[begin; a; rescue b; end]=*c" =>
    [[:attrasgn, [:vcall, :z], :[]=, [:array, [:rescue, [:vcall,
     :a], [:resbody, [:array, [:vcall, :b]]]], [:svalue, [:splat, [
    :vcall, :c]]]]]],

  "z[begin; a; rescue b; end]=c" =>
    [[:attrasgn, [:vcall, :z], :[]=, [:array, [:rescue, [:vcall,
     :a], [:resbody, [:array, [:vcall, :b]]]], [:vcall, :c]]]],

  "z{a=0}" =>
    [[:iter, [:fcall, :z], nil, [:dasgn_curr, :a, [:lit, 0]]]],

  "z{|a,*$b| d}" =>
    [[:iter, [:fcall, :z], [:masgn, [:array, [:dasgn_curr, :a]],
     [:gasgn, :$b], nil], [:vcall, :d]]],

  "z{|a,*b.c| d}" =>
    [[:iter, [:fcall, :z], [:masgn, [:array, [:dasgn_curr, :a]],
     [:attrasgn, [:vcall, :b], :c=], nil], [:vcall, :d]]],

  "z{|a,*b[c]| d}" =>
    [[:iter, [:fcall, :z], [:masgn, [:array, [:dasgn_curr, :a]],
     [:attrasgn, [:vcall, :b], :[]=, [:array, [:vcall, :c]]], nil], [
    :vcall, :d]]],

  "z{|a,a[b]| a.c;d}" =>
    [[:iter, [:fcall, :z], [:masgn, [:array, [:dasgn_curr, :a], [
    :attrasgn, [:dvar, :a], :[]=, [:array, [:vcall, :b]]]], nil,
     nil], [:block, [:call, [:dvar, :a], :c], [:vcall, :d]]]],

  "z{|a,a[b]| d}" =>
    [[:iter, [:fcall, :z], [:masgn, [:array, [:dasgn_curr, :a], [
    :attrasgn, [:dvar, :a], :[]=, [:array, [:vcall, :b]]]], nil,
     nil], [:vcall, :d]]],

  "z{|a,c[b],c| a.c;d}" =>
    [[:iter, [:fcall, :z], [:masgn, [:array, [:dasgn_curr, :a], [
    :attrasgn, [:vcall, :c], :[]=, [:array, [:vcall, :b]]], [
    :dasgn_curr, :c]], nil, nil], [:block, [:call, [:dvar, :a],
     :c], [:vcall, :d]]]],

  "z{|| p (1).m}" =>
    [[:iter, [:fcall, :z], 0, [:call, [:fcall, :p, [:array, [
    :lit, 1]]], :m]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "{!a=>b}" => [[:hash, [:not, [:vcall, :a]], [:vcall, :b]]],
  "{'one' => {}}" => [[:hash, [:str, "one"], [:hash]]],
  "{+a=>b}" => [[:hash, [:call, [:vcall, :a], :+@], [:vcall, :b]]],
  "{-a=>b}" => [[:hash, [:call, [:vcall, :a], :-@], [:vcall, :b]]],
  "{1, 2, 2, 4, 3, 6}" =>
    [[:hash, [:lit, 1], [:lit, 2], [:lit, 2], [:lit, 4], [:lit,
     3], [:lit, 6]]],

  "{44 => TypeValue = Type, ClassValue = Clalue => 55}" =>
    [[:hash, [:lit, 44], [:cdecl, :TypeValue, [:const, :Type]], [
    :cdecl, :ClassValue, [:const, :Clalue]], [:lit, 55]]],

  "{44 => TypeValue, ClassValue = Clalue => 55}" =>
    [[:hash, [:lit, 44], [:const, :TypeValue], [:cdecl, :ClassValue, [
    :const, :Clalue]], [:lit, 55]]],

  "{5=>a=b=1,d=2=>6}" =>
    [[:hash, [:lit, 5], [:lasgn, :a, [:lasgn, :b, [:lit, 1]]], [
    :lasgn, :d, [:lit, 2]], [:lit, 6]]],

  "{::A=>b}" => [[:hash, [:colon3, :A], [:vcall, :b]]],
  "{:a=>b}" => [[:hash, [:lit, :a], [:vcall, :b]]],
  "{:n!=>1}" => SyntaxError.new("(string):1: syntax error, unexpected '>'\n{:n!=>1}\n      ^"),
  "{:n==>1}" => [[:hash, [:lit, :n=], [:lit, 1]]],
  "{:n=>1}" => [[:hash, [:lit, :n], [:lit, 1]]],
  "{:n?=>1}" => SyntaxError.new("(string):1: syntax error, unexpected tASSOC\n{:n?=>1}\n      ^"),
  "{A=>b}" => [[:hash, [:const, :A], [:vcall, :b]]],
  "{a a=>b}" => SyntaxError.new("(string):1: syntax error, unexpected tIDENTIFIER, expecting kDO or '{' or '('\n{a a=>b}\n    ^"),
  "{a()=>b}" => [[:hash, [:fcall, :a], [:vcall, :b]]],
  "{a+a=>b}" =>
    [[:hash, [:call, [:vcall, :a], :+, [:array, [:vcall, :a]]], [
    :vcall, :b]]],

  "{a0 = a rescue b => c}" =>
    [[:hash, [:lasgn, :a0, [:rescue, [:vcall, :a], [:resbody, nil, [
    :vcall, :b]]]], [:vcall, :c]]],

  "{a=>b,c=>d}" =>
    [[:hash, [:vcall, :a], [:vcall, :b], [:vcall, :c], [:vcall,
     :d]]],

  "{a=>b}" => [[:hash, [:vcall, :a], [:vcall, :b]]],
  "{a=>p ().metho}" =>
    [[:hash, [:vcall, :a], [:call, [:fcall, :p], :metho]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "{a=>p (1).metho}" =>
    [[:hash, [:vcall, :a], [:call, [:fcall, :p, [:array, [:lit,
     1]]], :metho]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "{a=>p (1,2).metho}" =>
    [[:hash, [:vcall, :a], [:call, [:fcall, :p, [:array, [:lit,
     1], [:lit, 2]]], :metho]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "{a=b=1,d=2}" =>
    [[:hash, [:lasgn, :a, [:lasgn, :b, [:lit, 1]]], [:lasgn, :d,
     [:lit, 2]]]],

  "{a{}=>b}" =>
    [[:hash, [:iter, [:fcall, :a], nil], [:vcall, :b]]],

  "{begin; a; rescue b; end=>m}" =>
    [[:hash, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], [:vcall, :m]]],

  "{defined? a=>b}" =>
    [[:hash, [:defined, [:vcall, :a]], [:vcall, :b]]],

  "{m=>begin; a; rescue b; end}" =>
    [[:hash, [:vcall, :m], [:rescue, [:vcall, :a], [:resbody, [
    :array, [:vcall, :b]]]]]],

  "{not a=>b}" => SyntaxError.new("(string):1: syntax error, unexpected kNOT, expecting '}'\n{not a=>b}\n    ^"),
  "{p (1) => a}" =>
    [[:hash, [:fcall, :p, [:array, [:lit, 1]]], [:vcall, :a]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "{p (1) => b}" =>
    [[:hash, [:fcall, :p, [:array, [:lit, 1]]], [:vcall, :b]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "{p (1).m => a}" =>
    [[:hash, [:call, [:fcall, :p, [:array, [:lit, 1]]], :m], [
    :vcall, :a]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "{p (1).metho => 4}" =>
    [[:hash, [:call, [:fcall, :p, [:array, [:lit, 1]]], :metho],
     [:lit, 4]],
    {:warnings=>["(string):1: warning: don't put space before argument parentheses"]}],

  "{}" => [[:hash]],
  "{~a=>b}" => [[:hash, [:call, [:vcall, :a], :~], [:vcall, :b]]],
  "~a**2" =>
    [[:call, [:call, [:vcall, :a], :~], :**, [:array, [:lit, 2]]
    ]],

  "~begin; a; rescue b; end" =>
    [[:call, [:rescue, [:vcall, :a], [:resbody, [:array, [:vcall,
     :b]]]], :~]],

}
